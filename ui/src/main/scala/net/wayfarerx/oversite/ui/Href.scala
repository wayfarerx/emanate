/*
 * Href.scala
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

/**
 * Wrapper around a resolved, context-specific hypertext reference.
 *
 * @param value The resolved, context-specific hypertext reference.
 */
final class Href private(val value: String) extends AnyRef

/**
 * Factory for resolved, context-specific hypertext references.
 */
object Href {

  /**
   * Creates a new resolved, context-specific hypertext reference.
   *
   * @param pointer The pointer to resolve.
   * @param ctx     The context to resolve in.
   * @return A new resolved, context-specific hypertext reference.
   */
  def apply(pointer: Pointer[_ <: Pointer.Type])(implicit ctx: Context): IO[Href] =
    ctx.resolve(pointer) map (p => new Href(p.href))

}
