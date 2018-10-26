/*
 * Document.scala
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
 * A markup document.
 *
 * @param metadata The metadata of this document.
 * @param content  The main content of this document.
 * @param sections The subsections of this document.
 */
case class Document(metadata: Metadata, content: Vector[Markup.Block], sections: Vector[Document.Section]) {

  /** The title of this document. */
  def title: String = metadata.name.display

}

/**
 * Definitions associated with documents.
 */
object Document {

  /**
   * A section of a document.
   *
   * @param header   The header content of this section.
   * @param content  The main content of this section.
   * @param sections The subsections of this section.
   */
  case class Section(header: Vector[Markup.Inline], content: Vector[Markup.Block], sections: Vector[Section])

}
