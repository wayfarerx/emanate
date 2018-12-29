/*
 * Template.scala
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
package ui

import cats.effect.IO

import scalatags.Text.TypedTag

/**
 * Base type for page templates that can be published.
 *
 * @tparam T The type of entity being published.
 */
trait Template[-T <: AnyRef] extends Publisher[T] {

  /* Map an HTML tag to a string. */
  final override def publish(entity: T)(implicit ctx: Context): IO[String] =
    toHtml(entity) map (_.render)

  /**
   * Attempts to publish an entity as HTML.
   *
   * @param entity The entity to publish.
   * @param ctx The context to publish in.
   * @return The result of attempting to publish an entity as HTML.
   */
  def toHtml(entity: T)(implicit ctx: Context): IO[TypedTag[String]]

}
