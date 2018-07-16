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

package net.wayfarerx.oversite
package model

import java.io.InputStream
import java.nio.file.{Files, Path => JPath}

import io.Codec
import util.control.NoStackTrace

import cats.effect.IO

import laika.api.Parse
import laika.parse.markdown.Markdown
import laika.tree.{Documents, Elements}


/**
 * A utility for parsing markdown documents.
 *
 * @param environment The environment to parse in.
 */
final class Parser(environment: Environment) {

  import Parser._

  /**
   * Attempts to load the contents of a document from a stream.
   *
   * @param stream The path of the document to load.
   * @return The result of attempting to load a document from disk.
   */
  def parse(stream: InputStream): IO[Markup.Document] = for {
    _ <- IO.shift(environment.blocking)
    document <- IO(Laika.fromStream(stream))
    _ <- IO.shift(environment.compute)
    result <- readDocument(document)
  } yield result

  /**
   * Attempts to load the contents of a document from disk.
   *
   * @param path The path of the document to load.
   * @return The result of attempting to load a document from disk.
   */
  def parse(path: JPath): IO[Markup.Document] = for {
    _ <- IO.shift(environment.blocking)
    document <- IO(Files.isRegularFile(path)) flatMap {
      case true => IO(path.toFile) flatMap (f => IO(Laika.fromFile(f)))
      case _ => Problem.raise(s"Not a regular file: $path")
    }
    _ <- IO.shift(environment.compute)
    result <- readDocument(document)
  } yield result

  /**
   * Converts a laika document into an emanate document.
   *
   * @param document The document to convert.
   * @return The converted document.
   */
  private def readDocument(document: Documents.Document): IO[Markup.Document] =
    document.content.content match {
      case Elements.Header(1, header, _) +: Elements.Paragraph(description, _) +: remaining =>
        for {
          i <- readInlines(description)
          sr <- readSection(1, header, remaining)
          n <- Name(sr._1.header.map(_.strip).mkString) match {
            case Some(name) => IO.pure(name)
            case None => Problem.raise(s"Invalid document name: ${sr._1.header}")
          }
          _ <- sr._2 match {
            case Seq() => IO.pure(())
            case _ => Problem.raise(s"Invalid section structure: $document")
          }
        } yield {
          val (a, d) = i match {
            case dd :+ Markup.Text(t) =>
              t lastIndexOf '@' match {
                case index if index >= 0 =>
                  (Some(t drop index + 1): Option[String]) -> (dd :+ Markup.Text(t take index))
                case _ =>
                  (None: Option[String]) -> (dd :+ Markup.Text(t))
              }
            case dd =>
              (None: Option[String]) -> dd
          }
          Markup.Document(n, d, a, sr._1.content, sr._1.sections)
        }
      case invalid =>
        Problem.raise(s"Invalid document markup: $invalid.")
    }

  /**
   * Attempts to read the content of a single section.
   *
   * @param level  The level of the section.
   * @param header The header of the section.
   * @param next   The elements to extract the section's content from.
   * @return The result of attempting to read the content of a single section and any remaining blocks.
   */
  private def readSection(
    level: Int,
    header: Seq[Elements.Span],
    next: Seq[Elements.Block]
  ): IO[(Markup.Section, Seq[Elements.Block])] = {
    val nested = next takeWhile {
      case Elements.Header(l, _, _) if l > level => true
      case Elements.Header(_, _, _) => false
      case _ => true
    }
    val content = nested.takeWhile {
      case Elements.Header(_, _, _) => false
      case _ => true
    }
    for {
      h <- readInlines(header)
      c <- readBlocks(content)
      s <- readSections(nested drop content.size)
    } yield Markup.Section(level, h, c, s) -> next.drop(nested.size)
  }

  /**
   * Attempts to read all available sections from the specified blocks.
   *
   * @param blocks The blocks to read sections from.
   * @return The result of the attempt to read all available sections from the specified blocks.
   */
  private def readSections(blocks: Seq[Elements.Block]): IO[Vector[Markup.Section]] =
    blocks match {
      case Elements.Header(level, header, _) +: next =>
        for {
          sr <- readSection(level, header, next)
          ss <- readSections(sr._2)
        } yield sr._1 +: ss
      case Seq() =>
        IO.pure(Vector.empty)
    }

  /**
   * Attempts to read block markup from a laika block.
   *
   * @param block The block to read from.
   * @return The result of the attempt to read block markup from a laika block.
   */
  private def readBlock(block: Elements.Block): IO[Markup.Block] = block match {
    case Elements.Paragraph(content, _) =>
      readInlines(content) map Markup.Paragraph
    case Elements.CodeBlock(_, content, _) =>
      readInlines(content) map Markup.CodeBlock
    case Elements.QuotedBlock(content, _, _) =>
      readBlocks(content) map Markup.BlockQuote
    case Elements.Rule(_) =>
      IO.pure(Markup.HorizontalRule)
    case Elements.EnumList(content, _, _, _) =>
      ((IO.pure(Vector.empty): IO[Vector[Markup.List.Item]]) /:
        content.collect { case Elements.EnumListItem(c, _, _, _) => c }) { (result, item) =>
        result flatMap (r => readBlocks(item) map (i => r :+ Markup.List.Item(i)))
      } map Markup.List.Ordered
    case Elements.BulletList(content, _, _) =>
      ((IO.pure(Vector.empty): IO[Vector[Markup.List.Item]]) /:
        content.collect { case Elements.BulletListItem(c, _, _) => c }) { (result, item) =>
        result flatMap (r => readBlocks(item) map (i => r :+ Markup.List.Item(i)))
      } map Markup.List.Unordered
    case invalid =>
      Problem.raise(s"Unsupported block markup: $invalid.")
  }

  /**
   * Attempts to read multiple block markups from multiple laika blocks.
   *
   * @param blocks The blocks to read from.
   * @return The result of the attempt to read multiple block markups from multiple laika blocks.
   */
  private def readBlocks(blocks: Seq[Elements.Block]): IO[Vector[Markup.Block]] = blocks match {
    case head +: tail => readBlock(head) flatMap (h => readBlocks(tail) map (h +: _))
    case _ => IO.pure(Vector.empty[Markup.Block])
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
    case Elements.Literal(content, _) =>
      IO.pure(Markup.Code(Vector(Markup.Text(content))))
    case Elements.Emphasized(content, _) =>
      readInlines(content) map Markup.Emphasized
    case Elements.Strong(content, _) =>
      readInlines(content) map Markup.Strong
    case Elements.Image(alt, Elements.URI(src, _), _, _, title, _) =>
      Asset.Image(src).map(asset => IO.pure(Markup.Image(asset, title, Some(alt.trim) filterNot (_.isEmpty)))) getOrElse
        Problem.raise(s"Invalid image reference: $src.")
    case Elements.ExternalLink(content, target, title, _) =>
      readInlines(content) flatMap (readLink(target, title, _))
    case invalid =>
      Problem.raise(s"Unsupported inline markup: $invalid.")
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
      case toAsset if toAsset exists (c => c == '.' || c == ':') =>
        Asset(toAsset)(environment.assetTypes) map (a => IO.pure(Markup.Link.ToAsset(a, title, content))) getOrElse
          Problem.raise(s"Invalid asset link: $toAsset.")
      case toEntity =>
        val (entity, fragment) = toEntity lastIndexOf '#' match {
          case index if index > 0 => toEntity.take(index) -> Some(toEntity drop index + 1).filterNot(_.isEmpty)
          case _ => toEntity -> None
        }
        Entity[AnyRef](entity) map (e => IO.pure(Markup.Link.ToEntity(e, fragment, title, content))) getOrElse
          Problem.raise(s"Invalid entity link: $entity.")
    }

}

/**
 * Definitions associated with parsers.
 */
object Parser {

  /** The implicit UTF-8 codec. */
  private implicit val UTF8: Codec = Codec.UTF8

  /** The laika parser to use. */
  private val Laika = Parse as Markdown.strict

  /**
   * The type of exception produced when parsing problems are encountered.
   *
   * @param message The message that describes this problem.
   */
  final class Problem(message: String) extends RuntimeException(message) with NoStackTrace

  /**
   * Factory for parsing problems.
   */
  object Problem {

    /**
     * Creates a new problem.
     *
     * @param message The message that describes the new problem.
     * @return A new problem.
     */
    def apply(message: String): Problem =
      new Problem(message)

    /**
     * Raises a new problem.
     *
     * @tparam T The type of the result.
     * @param message The message that describes the new problem.
     * @return The raising of a new problem.
     */
    def raise[T](message: String): IO[T] =
      IO.raiseError(Problem(message))

  }

}
