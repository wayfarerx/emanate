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
trait Page {

  /**
   * Attempts to render a page as an HTML string.
   *
   * @param site     The site being rendered for.
   * @param document The document being rendered.
   * @param ctx      The context to render in.
   * @return The result of attempting to render the page as an HTML string.
   */
  final def apply(site: Site, document: Markup.Document)(implicit ctx: Context): IO[String] = for {
    head <- headTag(site, document)
    body <- bodyTag(site, document)
  } yield "<!DOCTYPE html>" + html(head, body).render

  /**
   * Attempts to render a head tag.
   *
   * @param site     The site being rendered for.
   * @param document The document being rendered.
   * @param ctx      The context to render in.
   * @return The result of attempting to render the head tag.
   */
  protected def headTag(site: Site, document: Markup.Document)(implicit ctx: Context): IO[TypedTag[String]] = for {
    image <- Name("image") map (n => ctx.resolve(Asset.Image(n))) getOrElse IO.pure(None)
    src <- image flatMap {
      case Asset.Relative(path, fileName, _) => Location(ctx.location.path ++ path) map (_ + fileName)
      case Asset.Absolute(location, fileName, _) => Some(location + fileName)
    } map (img => IO.pure(Some(site.baseUrl + img))) getOrElse IO.pure(None)
    alt <- image map ctx.alt getOrElse IO.pure(None)
    owner <- ctx.resolve(Author(site.owner))
    styles <- ctx.stylesheets
    stylesheets <- (IO.pure(Vector.empty[Styles.Explicit]) /: styles) { (results, stylesheet) =>
      results flatMap { r =>
        stylesheet match {
          case Styles.Internal(asset) =>
            ctx.resolve(asset) map (_ map (a => r :+ Styles.Explicit(a.href)) getOrElse r)
          case Styles.Generated(_href, _) =>
            IO.pure(r :+ Styles.Explicit(_href))
          case explicit@Styles.Explicit(_, _, _) =>
            IO.pure(r :+ explicit)
        }
      }
    }
  } yield {
    val title = document.name.display
    val description = document.descriptions.map(_.strip).mkString
    head(
      meta(charset := "utf-8"),
      meta(httpEquiv := "X-UA-Compatible", content := "IE=edge"),
      meta(name := "viewport", content := "width=device-width, initial-scale=1"),
      scalatags.Text.tags2.title(title),
      meta(name := "description", content := description),
      meta(name := "keywords", content := description),
      meta(name := "og:title", content := title),
      meta(name := "og:description", content := description),
      meta(name := "og:site_name", content := site.name.display),
      meta(name := "og:url", content := site.baseUrl + ctx.location),
      src map (s => meta(name := "og:image", content := s)),
      alt map (a => meta(name := "og:image:alt", content := a)),
      meta(name := "twitter:card", content := "summary_large_image"),
      owner flatMap (_.twitter) map (t => meta(name := "twitter:site", content := t)),
      document.author orElse owner flatMap (_.twitter) map (t => meta(name := "twitter:creator", content := t)),
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
   * @param site     The site being rendered for.
   * @param document The document being rendered.
   * @param ctx      The context to render in.
   * @return The result of attempting to render the body tag.
   */
  protected def bodyTag(site: Site, document: Markup.Document)(implicit ctx: Context): IO[TypedTag[String]] =
    pageContent(site, document) map (body(_))

  /**
   * Attempts to render the page content inside a body tag.
   *
   * @param site     The site being rendered for.
   * @param document The document being rendered.
   * @param ctx      The context to render in.
   * @return The result of attempting to render the page content inside a body tag.
   */
  protected def pageContent(site: Site, document: Markup.Document)(implicit ctx: Context): IO[Frag]

}
