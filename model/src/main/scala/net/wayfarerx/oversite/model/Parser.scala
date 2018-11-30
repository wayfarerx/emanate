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
import java.net.URL

import io.Codec
import util.control.NoStackTrace

import cats.effect.IO

import laika.ast

/**
 * A utility for parsing markdown documents.
 */
object Parser {

  /** The implicit UTF-8 codec. */
  private implicit val UTF8: Codec = Codec.UTF8

  /** The laika parser to use. */
  private val Laika = laika.api.Parse as laika.format.Markdown

  /**
   * Attempts to load the contents of a document from a stream.
   *
   * @param stream The path of the document to load.
   * @return The result of attempting to load a document from disk.
   */
  def parse(stream: InputStream): IO[Document] = for {
    markdown <- IO(Laika.fromStream(stream))
    result <- readDocument(markdown)
  } yield result

  /**
   * Attempts to load the contents of a document from disk.
   *
   * @param resource The path of the document to load.
   * @return The result of attempting to load a document from disk.
   */
  def parse(resource: URL): IO[Document] =
    IO(resource.openStream()).bracket(parse)(stream => IO(stream.close()))

  /**
   * Converts a laika document into an emanate document.
   *
   * @param markdown The document to convert.
   * @return The converted document.
   */
  private def readDocument(markdown: ast.Document): IO[Document] =
    markdown.content.content match {
      case ast.Title(header, _) +: ast.Paragraph(description, _) +: remaining =>
        for {
          i <- readInlines(description)
          s <- readSection(header, remaining)
          n <- Name(s.header.map(_.strip).mkString) match {
            case Some(name) => IO.pure(name)
            case None => Problem.raise(s"Invalid document title: ${s.header}")
          }
        } yield {
          val (a, d) = i match {
            case dd :+ Markup.Text(t) =>
              t lastIndexOf '@' match {
                case index if index >= 0 =>
                  (Some(t drop index + 1): Option[String]) ->
                    (dd :+ Markup.Text(t.take(index).replaceAll("\\s+$", "")))
                case _ =>
                  (None: Option[String]) -> i
              }
            case _ =>
              (None: Option[String]) -> i
          }
          Document(Metadata(n, a flatMap (Name(_)) map (Author(_)), d), s.content, s.sections)
        }
      case invalid =>
        Problem.raise(s"Document missing a description: $invalid.")
    }

  /**
   * Attempts to read the content of a single section.
   *
   * @param header The header of the section.
   * @param next   The elements to extract the section's content from.
   * @return The result of attempting to read the content of a single section and any remaining blocks.
   */
  private def readSection(
    header: Seq[ast.Span],
    next: Seq[ast.Block]
  ): IO[Document.Section] = {
    val content = next takeWhile {
      case ast.Section(_, _, _) => false
      case _ => true
    }
    for {
      h <- readInlines(header)
      c <- readBlocks(content)
      s <- readSections(next drop content.size collect { case section@ast.Section(_, _, _) => section })
    } yield Document.Section(h, c, s)
  }

  /**
   * Attempts to read all available sections from the specified blocks.
   *
   * @param sections The blocks to read sections from.
   * @return The result of the attempt to read all available sections from the specified blocks.
   */
  private def readSections(sections: Seq[ast.Section]): IO[Vector[Document.Section]] =
    sections match {
      case ast.Section(header, content, _) +: tail => for {
        s <- readSection(header.content, content)
        ss <- readSections(tail)
      } yield s +: ss
      case _ => IO.pure(Vector.empty)
    }

  /**
   * Attempts to read block markup from a laika block.
   *
   * @param block The block to read from.
   * @return The result of the attempt to read block markup from a laika block.
   */
  private def readBlock(block: ast.Block): IO[Markup.Block] = block match {
    case ast.Paragraph(content, _) =>
      readInlines(content) map Markup.Paragraph
    case ast.LiteralBlock(content, _) =>
      IO.pure(Markup.CodeBlock(Vector(Markup.Text(content))))
    case ast.QuotedBlock(content, _, _) =>
      readBlocks(content) map Markup.BlockQuote
    case ast.Rule(_) =>
      IO.pure(Markup.HorizontalRule)
    case ast.EnumList(content, _, _, _) =>
      ((IO.pure(Vector.empty): IO[Vector[Markup.List.Item]]) /:
        content.collect { case ast.EnumListItem(c, _, _, _) => c }) { (result, item) =>
        result flatMap (r => readBlocks(item) map (i => r :+ Markup.List.Item(i)))
      } map Markup.List.Ordered
    case ast.BulletList(content, _, _) =>
      ((IO.pure(Vector.empty): IO[Vector[Markup.List.Item]]) /:
        content.collect { case ast.BulletListItem(c, _, _) => c }) { (result, item) =>
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
  private def readBlocks(blocks: Seq[ast.Block]): IO[Vector[Markup.Block]] = blocks match {
    case head +: tail => readBlock(head) flatMap (h => readBlocks(tail) map (h +: _))
    case _ => IO.pure(Vector.empty[Markup.Block])
  }

  /**
   * Attempts to read inline markup from a laika span.
   *
   * @param span The span to read from.
   * @return The result of the attempt to read inline markup from a laika span.
   */
  private def readInline(span: ast.Span): IO[Markup.Inline] = span match {
    case ast.Text(content, _) =>
      IO.pure(Markup.Text(content))
    case ast.Literal(content, _) =>
      IO.pure(Markup.Code(Vector(Markup.Text(content))))
    case ast.Emphasized(content, _) =>
      readInlines(content) map Markup.Emphasized
    case ast.Strong(content, _) =>
      readInlines(content) map Markup.Strong
    case ast.Image(alt, ast.URI(src, _), _, _, title, _) =>
      IO.pure(Markup.Image(
        Pointer.Image.parse(src),
        title map (_.trim) filterNot (_.isEmpty),
        Some(alt.trim) filterNot (_.isEmpty)
      ))
    case ast.ExternalLink(content, target, title, _) =>
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
  private def readInlines(spans: Seq[ast.Span]): IO[Vector[Markup.Inline]] = spans match {
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
    target lastIndexOf '#' match {
      case 0 =>
        IO.pure(Markup.Link.Jump(target substring 1, title, content))
      case index if index > 0 =>
        IO.pure(Markup.Link.LoadAndJump(Pointer.parse(target take index), target drop index + 1, title, content))
      case _ =>
        IO.pure(Markup.Link.Load(Pointer.parse(target), title, content))
    }

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
