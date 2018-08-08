/*
 * Resolver.scala
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

import cats.effect.IO

/**
 * A utility for resolving markdown documents.
 *
 * @param context The context to operate in.
 *
 */
final class Resolver(context: Context) {

  /**
   * Attempts to resolve all authors, assets, and entities in the specified document.
   *
   * @param document The document to resolve.
   * @return The result of the attempt to resolve all authors, assets, and entities in the specified document.
   */
  def resolve(document: Markup.Document): IO[Markup.Document] = for {
    description <- resolveInlines(document.description)
    author <- document.author map (a => resolve(a) map (Some(_))) getOrElse IO.pure(None)
    content <- resolveBlocks(document.content)
    sections <- resolveSections(document.sections)
  } yield document.copy(description = description, author = author, content = content, sections = sections)

  /**
   * Resolves an author.
   *
   * @param author The author to resolve.
   * @return The resolved author.
   */
  private def resolve(author: Author): IO[Author] =
    context.resolve(author) map (_ getOrElse author)

  /**
   * Resolves an asset.
   *
   * @param asset The asset to resolve.
   * @return The resolved asset.
   */
  private def resolve[T <: Asset.Type](asset: Asset[T]): IO[Asset.Resolved[T]] =
    context.resolve(asset) flatMap {
      case Some(a) => IO.pure(a)
      case None => IO.raiseError(new IllegalArgumentException(s"In ${context.location} asset not found: $asset"))
    }

  /**
   * Resolves an entity.
   *
   * @param entity The entity to resolve.
   * @return The resolved entity.
   */
  private def resolve(entity: Entity[AnyRef]): IO[Entity.Resolved[AnyRef]] =
    context.resolve(entity) flatMap {
      case Some(e) => IO.pure(e)
      case None => IO.raiseError(new IllegalArgumentException(s"In ${context.location} entity not found: $entity"))
    }

  /**
   * Resolves a collection of inline markup elements.
   *
   * @param markup The markup to resolve.
   * @return The resolved collection of inline markup elements.
   */
  private def resolveInlines(markup: Vector[Markup.Inline]): IO[Vector[Markup.Inline]] = {

    /* Recursively resolve each inline markup element. */
    def next(remaining: Vector[Markup.Inline], results: Vector[Markup.Inline]): IO[Vector[Markup.Inline]] =
      remaining match {
        case head +: tail =>
          (head match {
            case text@Markup.Text(_) =>
              IO.pure(text)
            case Markup.Code(content) =>
              resolveInlines(content) map Markup.Code
            case Markup.Emphasized(content) =>
              resolveInlines(content) map Markup.Emphasized
            case Markup.Strong(content) =>
              resolveInlines(content) map Markup.Strong
            case Markup.Image(asset, title, alt) =>
              resolve(asset) map (Markup.Image(_, title, alt))
            case Markup.Link.Local(fragment, title, content) =>
              resolveInlines(content) map (Markup.Link.Local(fragment, title, _))
            case Markup.Link.ToEntity(entity, fragment, title, content) => for {
              e <- resolve(entity)
              c <- resolveInlines(content)
            } yield Markup.Link.ToEntity(e, fragment, title, c)
            case Markup.Link.ToAsset(asset, title, content) => for {
              a <- resolve(asset)
              c <- resolveInlines(content)
            } yield Markup.Link.ToAsset(a, title, c)
            case Markup.Link.External(href, title, content) =>
              resolveInlines(content) map (Markup.Link.External(href, title, _))
          }) flatMap (i => next(tail, results :+ i))
        case _ =>
          IO.pure(results)
      }

    next(markup, Vector.empty)
  }

  /**
   * Resolves a collection of block markup elements.
   *
   * @param markup The markup to resolve.
   * @return The resolved collection of block markup elements.
   */
  private def resolveBlocks(markup: Vector[Markup.Block]): IO[Vector[Markup.Block]] = {

    /* Recursively resolve each list item. */
    def list(remaining: Vector[Markup.List.Item], results: Vector[Markup.List.Item]): IO[Vector[Markup.List.Item]] =
      remaining match {
        case head +: tail => resolveBlocks(head.content) flatMap (c => list(tail, results :+ Markup.List.Item(c)))
        case _ => IO.pure(results)
      }

    /* Recursively resolve each block markup element. */
    def next(remaining: Vector[Markup.Block], results: Vector[Markup.Block]): IO[Vector[Markup.Block]] =
      remaining match {
        case head +: tail =>
          (head match {
            case hr@Markup.HorizontalRule =>
              IO.pure(hr)
            case Markup.Paragraph(content) =>
              resolveInlines(content) map Markup.Paragraph
            case Markup.CodeBlock(content) =>
              resolveInlines(content) map Markup.CodeBlock
            case Markup.BlockQuote(content) =>
              resolveBlocks(content) map Markup.BlockQuote
            case Markup.List.Ordered(items) =>
              list(items, Vector.empty) map Markup.List.Ordered
            case Markup.List.Unordered(items) =>
              list(items, Vector.empty) map Markup.List.Unordered
          }) flatMap (b => next(tail, results :+ b))
        case _ =>
          IO.pure(results)
      }

    next(markup, Vector.empty)
  }

  /**
   * Resolves a collection of section markup elements.
   *
   * @param markup The markup to resolve.
   * @return The resolved collection of section markup elements.
   */
  private def resolveSections(markup: Vector[Markup.Section]): IO[Vector[Markup.Section]] = {

    /* Recursively resolve each section markup element. */
    def next(remaining: Vector[Markup.Section], results: Vector[Markup.Section]): IO[Vector[Markup.Section]] =
      remaining match {
        case head +: tail => {
          for {
            header <- resolveInlines(head.header)
            content <- resolveBlocks(head.content)
            sections <- resolveSections(head.sections)
          } yield Markup.Section(head.level, header, content, sections)
        } flatMap (s => next(tail, results :+ s))
        case _ =>
          IO.pure(results)
      }

    next(markup, Vector.empty)
  }

}
