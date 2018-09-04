/*
 * Page.scala
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
package utils

import cats.effect.IO

import scalatags.Text.TypedTag
import scalatags.Text.all._

/**
 * A utility to assist with the rendering of pages
 */
trait Page[T] {

  /**
   * Attempts to render a page as an HTML string.
   *
   * @param entity The entity being rendered.
   * @param ctx    The context to render in.
   * @return The result of attempting to render the page as an HTML string.
   */
  final def apply(entity: T)(implicit ctx: Context): IO[String] = for {
    head <- headTag(entity)
    body <- bodyTag(entity)
  } yield "<!DOCTYPE html>" + html(head, body).render

  /**
   * Attempts to render a head tag.
   *
   * @param entity The entity being rendered.
   * @param ctx    The context to render in.
   * @return The result of attempting to render the head tag.
   */
  protected def headTag(entity: T)(implicit ctx: Context): IO[TypedTag[String]] = for {
    image <- ctx.resolve(pageImage(entity))
    src <- image flatMap {
      case Asset.Relative(path, fileName, _) => Location(ctx.location.path ++ path) map (_ + fileName)
      case Asset.Absolute(location, fileName, _) => Some(location + fileName)
    } map (img => IO.pure(Some(ctx.site.baseUrl + img))) getOrElse IO.pure(None)
    alt <- image map ctx.alt getOrElse IO.pure(None)
    owner <- ctx.resolve(Author(ctx.site.owner))
    styles <- ctx.stylesheets
    stylesheets <- (IO.pure(Vector.empty[Styles.Explicit]) /: styles) { (results, stylesheet) =>
      results flatMap { r =>
        stylesheet match {
          case Styles.Internal(asset) =>
            ctx.resolve(asset) map (_ map (a => r :+ Styles.Explicit(a.href)) getOrElse r)
          case Styles.Generated(n, _) =>
            IO.pure(r :+ Styles.Explicit(s"${ctx.location}$n.css"))
          case explicit@Styles.Explicit(_, _, _) =>
            IO.pure(r :+ explicit)
        }
      }
    }
  } yield {
    val title = pageName(entity).display
    val description = pageDescription(entity).map(_.strip).mkString
    val author = pageAuthor(entity) orElse owner
    val keywords = (s"$title $description ${author.map(_.name).getOrElse("")}" split
      """[\s-_\.,;:[]<>\(\)`'"]+""" filterNot (_.isEmpty) filterNot Page.IgnoredKeywords).distinct
    head(
      meta(charset := "utf-8"),
      meta(httpEquiv := "X-UA-Compatible", content := "IE=edge"),
      meta(name := "viewport", content := "width=device-width, initial-scale=1"),
      scalatags.Text.tags2.title(title),
      meta(name := "description", content := description),
      meta(name := "keywords", content := keywords mkString " "),
      meta(name := "og:title", content := title),
      meta(name := "og:description", content := description),
      meta(name := "og:site_name", content := ctx.site.name.display),
      meta(name := "og:url", content := ctx.site.baseUrl + ctx.location),
      src map (s => meta(name := "og:image", content := s)),
      alt map (a => meta(name := "og:image:alt", content := a)),
      meta(name := "twitter:card", content := "summary_large_image"),
      owner flatMap (_.twitter) map (t => meta(name := "twitter:site", content := t)),
      author flatMap (_.twitter) map (t => meta(name := "twitter:creator", content := t)),
      stylesheets map {
        case Styles.Explicit(_href, None, None) =>
          link(rel := "stylesheet", href := _href)
        case Styles.Explicit(_href, Some(integrity), None) =>
          link(rel := "stylesheet", href := _href, attr("integrity") := integrity)
        case Styles.Explicit(_href, None, Some(crossorigin)) =>
          link(rel := "stylesheet", href := _href, attr("crossorigin") := crossorigin)
        case Styles.Explicit(_href, Some(integrity), Some(crossorigin)) =>
          link(rel := "stylesheet", href := _href, attr("integrity") := integrity, attr("crossorigin") := crossorigin)
      }
    )
  }

  /**
   * Attempts to render a body tag.
   *
   * @param entity The entity being rendered.
   * @param ctx    The context to render in.
   * @return The result of attempting to render the body tag.
   */
  protected def bodyTag(entity: T)(implicit ctx: Context): IO[TypedTag[String]] =
    pageContent(entity) map (body(_))

  /**
   * Returns the name of a page.
   *
   * @param entity The entity being rendered.
   * @return The name of the page.
   */
  protected def pageName(entity: T): Name

  /**
   * Returns the description of a page.
   *
   * @param entity The entity being rendered.
   * @return The description of the page.
   */
  protected def pageDescription(entity: T): Vector[Markup.Inline] = Vector.empty

  /**
   * Returns the author of a page.
   *
   * @param entity The entity being rendered.
   * @return The author of the page.
   */
  protected def pageAuthor(entity: T): Option[Author] = None

  /**
   * Returns the author of a page.
   *
   * @param entity The entity being rendered.
   * @return The author of the page.
   */
  protected def pageImage(entity: T): Asset[Asset.Image] = Asset.Image(Name("image").get)

  /**
   * Attempts to render the page content inside a body tag.
   *
   * @param entity The entity being rendered.
   * @param ctx    The context to render in.
   * @return The result of attempting to render the page content inside a body tag.
   */
  protected def pageContent(entity: T)(implicit ctx: Context): IO[Frag]

}

object Page {

  private val IgnoredKeywords = Set.empty[String]

  implicit final class ImageRenderer(val image: Asset.Resolved[Asset.Image]) extends AnyVal {

    def renderImg(implicit ctx: Context): IO[Frag] =
      ctx.alt(image) map (a => img(src := image.href, a map (alt := _) getOrElse frag()))

  }

}