package net.wayfarerx.oversite
package utils

import cats.effect.IO
import scalatags.Text.TypedTag
import scalatags.Text.all._

trait Page {

  final def apply(site: Site, document: Markup.Document)(implicit ctx: Context): IO[String] = for {
    head <- headTag(site, document)
    body <- bodyTag(site, document)
  } yield "<!DOCTYPE html>" + html(head, body).render

  def headTag(site: Site, document: Markup.Document)(implicit ctx: Context): IO[TypedTag[String]] = for {
    image <- Name("image") map (n => ctx.resolve(Asset.Image(n))) getOrElse IO.pure(None)
    src <- image flatMap {
      case Asset.Relative(path, fileName, _) => Location(ctx.location.path ++ path) map (_ + fileName)
      case Asset.Absolute(location, fileName, _) => Some(location + fileName)
    } map (img => IO.pure(Some(site.baseUrl + img))) getOrElse IO.pure(None)
    alt <- image map ctx.alt getOrElse IO.pure(None)
    owner <- ctx.resolve(Author(site.owner))
  } yield {
    val title = document.name.display
    val description = document.description.map(_.strip).mkString
    head(
      meta(charset := Page.Charset.toLowerCase),
      meta(httpEquiv := "X-UA-Compatible", content := "IE=edge"),
      meta(name := "viewport", content := "width=device-width, initial-scale=1"),
      scalatags.Text.tags2.title(title),
      meta(name := "description", content := description),
      meta(name := "keywords", content := description),
      meta(name := "og:title", content := title),
      meta(name := "og:description", content := description),
      meta(name := "og:site_name", content := site.name.display),
      meta(name := "og:url", content := site.baseUrl + ctx.location),
      src map (s => frag(meta(name := "og:image", content := s))) getOrElse frag(),
      alt map (a => frag(meta(name := "og:image:alt", content := a))) getOrElse frag(),
      meta(name := "twitter:card", content := "summary_large_image"),
      owner flatMap (o => o.twitter map (t => meta(name := "twitter:site", content := t))) getOrElse frag()
    )
  }

  def bodyTag(site: Site, document: Markup.Document)(implicit ctx: Context): IO[TypedTag[String]]

}

object Page {

  /** The codec to use. */
  private val Charset = "UTF-8"

}