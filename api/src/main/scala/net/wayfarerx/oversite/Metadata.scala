/*
 * Metadata.scala
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

/**
 * Document metadata.
 *
 * @param name        The name of the document.
 * @param author      The optional author of the document.
 * @param description The optional description of the document.
 * @param image       The optional image for the document.
 */
case class Metadata(
  name: Name,
  author: Option[Author] = None,
  description: Vector[Markup.Inline] = Vector.empty,
  image: Option[Pointer.Internal[Pointer.Image]] = Some(Pointer.Image(Pointer.Image.name))
)
