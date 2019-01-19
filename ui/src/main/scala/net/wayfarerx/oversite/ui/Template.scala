/*
 * Template.scala
 *
 * Copyright 2018-2019 wayfarerx <x@wayfarerx.net> (@thewayfarerx)
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
package ui

import cats.effect.IO

import scalatags.Text.TypedTag
import scalatags.Text.all._

/**
 * Base type for page templates that can be published.
 *
 * @tparam T The type of entity being published.
 */
trait Template[-T <: AnyRef] extends Publisher[T] with Renderers {

  /** The doctype to prepend to the HTML page. */
  def doctype: String = "<!DOCTYPE html>"

  /**
   * Attempts to publish an entity as HTML.
   *
   * @param entity The entity to publish.
   * @param ctx    The context to publish in.
   * @return The result of attempting to publish an entity as HTML.
   */
  def toHtml(entity: T)(implicit ctx: Context): IO[TypedTag[String]]

  /* Map an HTML tag to a string. */
  final override def publish(entity: T)(implicit ctx: Context): IO[String] =
    toHtml(entity) map (doctype + _.render)

}

/**
 * Definitions associated with templates.
 */
object Template {

  /**
   * Useful base class for templates.
   *
   * @tparam T The type of entity being published.
   */
  trait Support[-T <: AnyRef] extends Template[T] {

    /* Compose the HTML page. */
    override def toHtml(entity: T)(implicit ctx: Context): IO[TypedTag[String]] =
      htmlContent(entity) map { case (_head, _body) => html(_head, _body) }

    /**
     * Generates the content of the `html` tag.
     *
     * @param entity The entity to generate for.
     * @param ctx    The context to generate in.
     * @return The content of the `html` tag.
     */
    protected def htmlContent(entity: T)(implicit ctx: Context): IO[(TypedTag[String], TypedTag[String])] =
      toHead(entity) flatMap (_head => toBody(entity) map (_body => _head -> _body))

    /**
     * Generates the `head` tag.
     *
     * @param entity The entity to generate for.
     * @param ctx    The context to generate in.
     * @return The `head` tag.
     */
    protected def toHead(entity: T)(implicit ctx: Context): IO[TypedTag[String]] =
      headContent(entity) map (head(_))

    /**
     * Generates the content of the `head` tag.
     *
     * @param entity The entity to generate for.
     * @param ctx    The context to generate in.
     * @return The content of the `head` tag.
     */
    protected def headContent(entity: T)(implicit ctx: Context): IO[Frag] =
      defaultHeadContent(entity)

    /**
     * Generates the `body` tag.
     *
     * @param entity The entity to generate for.
     * @param ctx    The context to generate in.
     * @return The `body` tag.
     */
    protected def toBody(entity: T)(implicit ctx: Context): IO[TypedTag[String]] =
      display(entity) map (body(_))

    /**
     * Returns the metadata for an entity.
     *
     * @param entity The entity to return the metadata for.
     * @param ctx    The context being published in.
     * @return The metadata for an entity.
     */
    protected def metadata(entity: T)(implicit ctx: Context): IO[Metadata] =
      ctx.describe(ctx.self)

    /**
     * Returns the scripts that this template references.
     *
     * @param entity The entity to return the scripts for.
     * @param ctx    The context being published in.
     * @return The scripts that this template references.
     */
    protected def scripts(entity: T)(implicit ctx: Context): IO[Vector[Reference]] =
      defaultAssetReferences(Pointer.Script, ctx.location)

    /**
     * Returns the stylesheets that this template references.
     *
     * @param entity The entity to return the stylesheets for.
     * @param ctx    The context being published in.
     * @return The stylesheets that this template references.
     */
    protected def stylesheets(entity: T)(implicit ctx: Context): IO[Vector[Reference]] =
      defaultAssetReferences(Pointer.Stylesheet, ctx.location)

    /**
     * Create the content of the `body` tag.
     *
     * @param entity The entity being published.
     * @param ctx    The context being published in.
     * @return The content of the `body` tag.
     */
    protected def display(entity: T)(implicit ctx: Context): IO[Frag]

    /**
     * Create the default content of the `head` tag.
     *
     * @param entity The entity being published.
     * @param ctx    The context being published in.
     * @return The default content of the `head` tag.
     */
    final private def defaultHeadContent(entity: T)(implicit ctx: Context): IO[Frag] = for {
      _metadata <- metadata(entity)
      _image <- _metadata.image map (ctx resolve _ map (Some(_))) getOrElse
        ctx.resolve(Pointer.Image(Pointer.Image.name)).redeem(_ => None, Some(_))
      _alt <- _image map ctx.alt getOrElse IO.pure(None)
      _favicon <- ctx.resolve(ctx.site.icon)
      _stylesheets <- stylesheets(entity)
      _scripts <- scripts(entity)
    } yield {
      val title = _metadata.name.display
      val author = _metadata.author getOrElse ctx.site.owner
      val description = _metadata.description.map(_.strip).mkString
      val keywords = s"$title ${author.name.display} $description" split """[_,;:<>`\s\-\.\[\]\(\)\'\"]+"""
      val src = _image flatMap {
        case Pointer.Target(_, prefix, suffix) =>
          Some(ctx.site.baseUrl + (prefix match {
            case Pointer.Prefix.Absolute(location) => location
            case Pointer.Prefix.Relative(path) => Location.resolved(ctx.location.path ++ path)
          }) + suffix)
        case _ => None
      }
      frag(
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
        src flatMap (_ => _alt map (a => meta(name := "og:image:alt", content := a))),
        meta(name := "twitter:card", content := src map (_ => "summary_large_image") getOrElse "summary"),
        ctx.site.owner.twitter map (t => meta(name := "twitter:site", content := t)),
        author.twitter map (t => meta(name := "twitter:creator", content := t)),
        link(
          rel := "shortcut icon",
          href := _favicon.href
        ),
        _stylesheets map (ref => link(
          rel := "stylesheet",
          href := ref.href.value,
          ref.integrity map (attr("integrity") := _),
          ref.crossorigin map (attr("crossorigin") := _)
        )),
        _scripts map (ref => script(
          `type` := "text/javascript",
          scalatags.Text.attrs.src := ref.href.value,
          ref.integrity map (attr("integrity") := _),
          ref.crossorigin map (attr("crossorigin") := _),
          attr("defer").empty
        ))
      )
    }

    /**
     * Returns the default assets of the specified type.
     *
     * @param asset    The type of assets to return.
     * @param location The location to search up from.
     * @param ctx      The context to resolve pointers with.
     * @return The default assets of the specified type.
     */
    final private def defaultAssetReferences(
      asset: Pointer.Asset,
      location: Location
    )(
      implicit ctx: Context
    ): IO[Vector[Reference]] = {
      val base = s"${asset.prefix map (Path(_)) getOrElse Path.empty}${asset.name}."

      /* Find the first asset that exists. */
      def find(remaining: List[String]): IO[Option[Href]] = remaining match {
        case ext :: tail => for {
          h <- Href(asset(location, base + ext)).redeem(_ => None, Some(_))
          r <- h map (_ => IO.pure(h)) getOrElse find(tail)
        } yield r
        case Nil =>
          IO.pure(None)
      }

      for {
        h <- find(asset.variants.flatMap(_.extensions.toNonEmptyList).map(_.normal).toList)
        p <- location.parent map (defaultAssetReferences(asset, _)) getOrElse IO.pure(Vector.empty)
      } yield h map (p :+ Reference(_)) getOrElse p
    }

  }

  /**
   * Representation of a `script` or `link` tag's href, integrity and cross-origin settings.
   *
   * @param href        The target of this reference.
   * @param integrity   The value of the `integrity` attribute.
   * @param crossorigin The value of the `crossorigin` attribute.
   */
  case class Reference(
    href: Href,
    integrity: Option[String] = None,
    crossorigin: Option[String] = None
  )

}
