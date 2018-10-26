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
import scalatags.Text.{Modifier, TypedTag}
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
  protected def headTag(entity: T)(implicit ctx: Context): IO[TypedTag[String]] = {
    val metadata = pageMetadata(entity)
    val title = metadata.name.display
    val description = metadata.description.map(_.strip).mkString
    val author = metadata.author
    val keywords = s"$title $description ${author map (_.name.display) getOrElse ""}"
      .split("""[_,;:<>`\s\-\.\[\]\(\)\'\"]+""")
      .filterNot(k => Page.IgnoredKeywords(k.toLowerCase)).distinct
    val img = pageImage(entity)
    for {
      owner <- ctx.resolve(Author(ctx.site.owner))
      styles <- ctx.stylesheets
      stylesheets <- (IO.pure(Vector.empty[Styles.Explicit]) /: styles) { (results, stylesheet) =>
        results flatMap { r =>
          stylesheet match {
            case Styles.Internal(asset) =>
              ctx.resolve(asset) map (_ map (a => r :+ Styles.Explicit(a.href)) getOrElse r)
            case explicit@Styles.Explicit(_, _, _) =>
              IO.pure(r :+ explicit)
          }
        }
      }
      image <- ctx.resolve(img.asset)
      src <- image flatMap {
        case Asset.Relative(path, fileName, _) => Location(ctx.location.path ++ path) map (_ + fileName)
        case Asset.Absolute(location, fileName, _) => Some(location + fileName)
      } map (abs => IO.pure(Some(ctx.site.baseUrl + abs))) getOrElse IO.pure(None)
      alt <- img.alt map (a => IO.pure(Some(a))) orElse (image map ctx.alt) getOrElse IO.pure(None)
    } yield head(
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
      stylesheets map (stylesheet => link(
        rel := "stylesheet",
        href := stylesheet.href,
        stylesheet.integrity map (attr("integrity") := _),
        stylesheet.crossorigin map (attr("crossorigin") := _)
      )))
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
   * Returns the metadata of a page.
   *
   * @param entity The entity being rendered.
   * @return The metadata of the page.
   */
  protected def pageMetadata(entity: T): Markup.Metadata

  /**
   * Returns the author of a page.
   *
   * @param entity The entity being rendered.
   * @return The author of the page.
   */
  protected def pageImage(entity: T): Markup.Image =
    Markup.Image(Asset.Image(name"image"), None, None)

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

  private val IgnoredKeywords = Set(
    "",
    "net",
    "of",
    "the"
  )

  private def resolveImageData(
    image: Asset[Asset.Image]
  )(
    implicit ctx: Context
  ): IO[Option[(Asset.Resolved[Asset.Image], Option[String])]] = {
    for {
      _img <- image match {
        case resolved: Asset.Resolved[Asset.Image] => IO.pure(Some(resolved))
        case other => ctx.resolve(other)
      }
      _alt <- _img map ctx.alt getOrElse IO.pure(None)
    } yield _img map (_ -> _alt)
  }

  implicit final class ImageAssetRenderer(val image: Asset[Asset.Image]) extends AnyVal {

    def renderImg(xs: Modifier*)(implicit ctx: Context): IO[Frag] =
      resolveImageData(image) map {
        case Some((_img, _alt)) => img(src := _img.href, _alt map (alt := _), xs)
        case _ => frag()
      }

  }

  implicit final class ImageMarkupRenderer(val image: Markup.Image) extends AnyVal {

    def renderImg(xs: Modifier*)(implicit ctx: Context): IO[Frag] =
      resolveImageData(image.asset) map {
        case Some((_img, _alt)) =>
          img(src := _img.href, image.alt orElse _alt map (alt := _), image.title map (title := _), xs)
        case _ => frag()
      }

  }

}