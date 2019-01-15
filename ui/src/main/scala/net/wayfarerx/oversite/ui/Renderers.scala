/*
 * Renderers.scala
 *
 * Copyright 2019 wayfarerx <x@wayfarerx.net> (@thewayfarerx)
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

import language.implicitConversions

import cats.effect.IO

import scalatags.Text.Modifier
import scalatags.Text.all._

/**
 * Extendable extensions that support rendering HTML.
 */
trait Renderers {

  /** Returns the renderer for the specified image pointer. */
  implicit def imagePointerRenderer(pointer: Pointer[Pointer.Image]): Renderers.ImagePointerRenderer =
    new Renderers.ImagePointerRenderer(pointer)

  /** Returns the renderer for the specified image markup. */
  implicit def imageMarkupRenderer(markup: Markup.Image): Renderers.ImageMarkupRenderer =
    new Renderers.ImageMarkupRenderer(markup)

}

/**
 * Importable extensions that support rendering HTML.
 */
object Renderers {

  /**
   * A renderer for image pointers.
   *
   * @param pointer The pointer to render.
   */
  implicit final class ImagePointerRenderer(val pointer: Pointer[Pointer.Image]) extends AnyVal {

    /**
     * Renders the underlying pointer.
     *
     * @param extra The extra modifiers to append.
     * @param ctx The context to render in.
     * @return The rendered HTML.
     */
    def render(extra: Modifier*)(implicit ctx: Context): IO[Frag] = for {
      _src <- ctx.resolve(pointer)
      _alt <- ctx.alt(_src)
    } yield img(src := _src.href, _alt map (alt := _), extra)

  }

  /**
   * A renderer for image markup.
   *
   * @param markup The markup to render.
   */
  implicit final class ImageMarkupRenderer(val markup: Markup.Image) extends AnyVal {

    /**
     * Renders the underlying markup.
     *
     * @param extra The extra modifiers to append.
     * @param ctx The context to render in.
     * @return The rendered HTML.
     */
    def render(extra: Modifier*)(implicit ctx: Context): IO[Frag] = for {
      _src <- ctx.resolve(markup.pointer)
      _alt <- markup.alt map (a => IO.pure(Some(a))) getOrElse ctx.alt(_src)
    } yield img(src := _src.href, _alt map (alt := _), markup.title map (title := _), extra)

  }

}
