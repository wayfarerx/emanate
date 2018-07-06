/*
 * Parser.scala
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
package model

import io.Codec

import cats.effect.IO

import laika.tree.Documents

object Parser {

  /** The UTF-8 codec. */
  private implicit val codec: Codec = Codec.UTF8

  def apply(document: Documents.Document): IO[Document] = {


    ???
  }

}
