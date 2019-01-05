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
trait Template[-T <: AnyRef] extends Publisher[T] {

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
    toHtml(entity).map(doctype + _.render)

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
    final override def toHtml(entity: T)(implicit ctx: Context): IO[TypedTag[String]] = for {
      _head <- headContent(entity)
      _body <- toBody(entity)
    } yield html(head(_head), _body)

    /**
     * Create the content of the `head` tag.
     *
     * @param entity The entity being published.
     * @param ctx    The context being published in.
     * @return The content of the `head` tag.
     */
    def headContent(entity: T)(implicit ctx: Context): IO[Seq[Frag]] = for {
      _metadata <- metadata(entity)
      _image <- _metadata.image map (ctx resolve _ map (Some(_))) getOrElse
        ctx.resolve(Pointer.Image(Pointer.Image.name)).redeem(_ => None, Some(_))
      _alt <- _image map ctx.alt getOrElse IO.pure(None)
      _scripts <- scripts(entity)
      _stylesheets <- stylesheets(entity)
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
      Seq(frag(
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
        _scripts map (ref => script(
          `type` := "text/javascript",
          scalatags.Text.attrs.src := ref.href.value,
          ref.integrity map (attr("integrity") := _),
          ref.crossorigin map (attr("crossorigin") := _),
          attr("defer").empty
        )),
        _stylesheets map (ref => link(
          rel := "stylesheet",
          href := ref.href.value,
          ref.integrity map (attr("integrity") := _),
          ref.crossorigin map (attr("crossorigin") := _)
        ))
      ))
    }

    /**
     * Returns the metadata for an entity.
     *
     * @param entity The entity to return the metadata for.
     * @param ctx    The context being published in.
     * @return The metadata for an entity.
     */
    def metadata(entity: T)(implicit ctx: Context): IO[Metadata]

    /**
     * Returns the scripts that this template references.
     *
     * @param entity The entity to return the scripts for.
     * @param ctx    The context being published in.
     * @return The scripts that this template references.
     */
    def scripts(entity: T)(implicit ctx: Context): IO[Vector[Reference]] =
      defaults(Pointer.Script, ctx.location)

    /**
     * Returns the stylesheets that this template references.
     *
     * @param entity The entity to return the stylesheets for.
     * @param ctx    The context being published in.
     * @return The stylesheets that this template references.
     */
    def stylesheets(entity: T)(implicit ctx: Context): IO[Vector[Reference]] =
      defaults(Pointer.Stylesheet, ctx.location)

    /**
     * Returns the default assets of the specified type.
     *
     * @param asset    The type of assets to return.
     * @param location The location to search up from.
     * @param ctx      The context to resolve pointers with.
     * @return The default assets of the specified type.
     */
    private def defaults(asset: Pointer.Asset, location: Location)(implicit ctx: Context): IO[Vector[Reference]] = for {
      h <- Href(asset(location, s"${asset.prefix map (_ + "/") getOrElse ""}${asset.name}.${asset.extension}"))
        .redeem(_ => None, Some(_))
      p <- location.parent map (defaults(asset, _)) getOrElse IO.pure(Vector.empty)
    } yield h map (p :+ Reference(_)) getOrElse p

    /**
     * Create the `body` tag.
     *
     * @param entity The entity being published.
     * @param ctx    The context being published in.
     * @return The `body` tag.
     */
    def toBody(entity: T)(implicit ctx: Context): IO[TypedTag[String]]

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