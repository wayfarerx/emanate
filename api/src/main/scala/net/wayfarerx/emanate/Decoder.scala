/*
 * Decoder.scala
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

package net.wayfarerx.emanate

import cats.effect.IO

/**
 * Defines how an entity is decoded from a document.
 *
 * @tparam T The type of entity being decoded.
 */
trait Decoder[+T <: AnyRef] {

  /**
   * Attempts to decode an entity from a document.
   *
   * @param document The document to decode.
   * @param ctx The context to decode in.
   * @return The result of attempting to decode an entity from a document.
   */
  def decode(document: Markup.Document)(implicit ctx: Context): IO[T]

}
