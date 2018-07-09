/*
 * Parser.scala
 *
 * Copyright 2018 wayfarerx <x@wayfarerx.net> (@thewayfarerx)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.wayfarerx.emanate
package model

import java.io.InputStream
import java.nio.file.{Files, Path => JPath}

import collection.immutable.ListSet
import io.Codec

import cats.effect.IO

import laika.api.Parse
import laika.parse.markdown.Markdown
import laika.tree.{Documents, Elements}


/**
 * A utility for parsing markdown documents.
 */
final class Parser(assetTypes: Asset.Type*) {

  /** The implicit UTF-8 codec. */
  private implicit val codec: Codec = Codec.UTF8

  /** The laika parser to use. */
  private val parser = Parse as Markdown.strict

  /** The set of all known asset types. */
  private val allAssetTypes = ListSet(Asset.Image, Asset.Stylesheet, Asset.Script) ++ assetTypes

  /** The index of asset types by name. */
  private val assetTypesByName = allAssetTypes.toVector.map(t => t.name -> t).toMap

  /** The index of asset types by extension. */
  private val assetTypesByExtension = allAssetTypes.toVector.flatMap(t => t.extensions map (_ -> t)).toMap

  /**
   * Attempts to load the contents of a document from a stream.
   *
   * @param stream The path of the document to load.
   * @return The result of attempting to load a document from disk.
   */
  def apply(stream: InputStream): IO[Markup.Document] =
    IO(parser.fromStream(stream)).flatMap(readDocument)

  /**
   * Attempts to load the contents of a document from disk.
   *
   * @param path The path of the document to load.
   * @return The result of attempting to load a document from disk.
   */
  def apply(path: JPath): IO[Markup.Document] =
    IO(Files.isRegularFile(path)).flatMap {
      case true => IO(Files.newInputStream(path)).bracket(apply)(input => IO(input.close()))
      case _ => IO.raiseError(new IllegalArgumentException(s"Not a regular file: $path"))
    }

  /**
   * Converts a laika document into an emanate document.
   *
   * @param document The document to convert.
   * @return The converted document.
   */
  private def readDocument(document: Documents.Document): IO[Markup.Document] = {
    /*
    document match {
      case HasDocumentHeader(header, description, author, remaining) =>

        ???
      case _ =>
        IO.raiseError(new IllegalArgumentException(s"Document header not found: $document."))
    }*/

    println(document)
    IO.pure(Markup.Document(Name("test").get, Vector.empty, None, Vector.empty, Vector.empty))
  }

  /**
   * Attempts to read inline markup from a laika span.
   *
   * @param span The span to read from.
   * @return The result of the attempt to read inline markup from a laika span.
   */
  private def readInline(span: Elements.Span): IO[Markup.Inline] = span match {
    case Elements.Text(content, _) =>
      IO.pure(Markup.Text(content))
    case Elements.Code(_, content, _) =>
      readInlines(content) map Markup.Code
    case Elements.Emphasized(content, _) =>
      readInlines(content) map Markup.Emphasized
    case Elements.Strong(content, _) =>
      readInlines(content) map Markup.Strong
    case Elements.Image(alt, Elements.URI(src, _), _, _, title, _) =>
      Asset.Image(src).map(asset => IO.pure(Markup.Image(asset, title, Some(alt.trim) filterNot (_.isEmpty))))
        .getOrElse(IO.raiseError(new IllegalArgumentException(s"Invalid image reference: $src.")))
    case Elements.ExternalLink(content, target, title, _) =>
      readInlines(content) flatMap (readLink(target, title, _))
    case _ =>
      IO.raiseError(new IllegalArgumentException(s"Unsupported inline markup: $span."))
  }

  /**
   * Attempts to read multiple inline markups from multiple laika spans.
   *
   * @param spans The spans to read from.
   * @return The result of the attempt to read multiple inline markups from multiple laika spans.
   */
  private def readInlines(spans: Seq[Elements.Span]): IO[Vector[Markup.Inline]] = spans match {
    case head +: tail => readInline(head) flatMap (h => readInlines(tail) map (h +: _))
    case _ => IO.pure(Vector.empty[Markup.Inline])
  }

  /**
   * Attempts to read a link markup item.
   *
   * @param target  The target of the link.
   * @param title   The title of the link if specified.
   * @param content The content the link contains.
   * @return The result of the attempt to read a link markup item.
   */
  private def readLink(target: String, title: Option[String], content: Vector[Markup.Inline]): IO[Markup.Link] =
    target match {
      case local if local startsWith "#" =>
        IO.pure(Markup.Link.Local(local substring 1, title, content))
      case external if external.startsWith("//") | external.contains("://") =>
        IO.pure(Markup.Link.External(external, title, content))
      case direct if direct contains ":" =>
        val Array(assetType, assetName) = direct.split("""\:""", 2)
        Name(assetType) flatMap assetTypesByName.get map { assets =>
          Name(assetName) map (name => IO.pure(Markup.Link.Direct(assets(name), title, content))) getOrElse
            IO.raiseError(new IllegalArgumentException(s"Invalid asset link name: $assetName."))
        } getOrElse IO.raiseError(new IllegalArgumentException(s"Invalid asset link type: $assetType."))
      case internal =>
        val (entity, fragment) = internal lastIndexOf '#' match {
          case index if index > 0 => internal.take(index - 1) -> Some(internal.drop(index + 1))
          case _ => internal -> None
        }
        entity match {
          case absolute if absolute startsWith "/" =>
            Location(Path(absolute)) map
              (l => IO.pure(Markup.Link.Internal(Entity.Absolute[AnyRef](l), fragment, title, content))) getOrElse
              IO.raiseError(new IllegalArgumentException(s"Invalid link absolute path: $absolute."))
          case relative if relative contains "/" =>
            IO.pure(Markup.Link.Internal(Entity.Relative(Path(relative)), fragment, title, content))
          case named =>
            Name(named) map
              (n => IO.pure(Markup.Link.Internal(Entity.Named(n), fragment, title, content))) getOrElse
              IO.raiseError(new IllegalArgumentException(s"Invalid link name: $named."))
        }
    }

  /**
   * Extractor for the common document header.
   */
  private object HasDocumentHeader {

    /**
     * Extracts the common document header from a laika document.
     *
     * @param document The document to extract from.
     * @return The extracted common document header.
     */
    def unapply(
      document: Documents.Document
    ): Option[(Elements.Header, Elements.Paragraph, Option[Name], Seq[Elements.Block])] =
      document.content.content match {
        case (header@Elements.Header(1, _, _)) +: (description@Elements.Paragraph(_, _)) +:
          Elements.Paragraph(Seq(Elements.Text(byLine, _)), _) +: remaining if byLine startsWith "by " =>
          Some(header, description, Name(byLine substring 3), remaining)
        case (header@Elements.Header(1, _, _)) +: (description@Elements.Paragraph(_, _)) +: remaining =>
          Some(header, description, None, remaining)
        case _ =>
          None
      }

  }

}
