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

package net.wayfarerx.emanate

/**
 * Representation of a document in the site.
 *
 * @param name The name of the document.
 * @param description The description of the document.
 */
case class Document(
  name: Name,
  description: Document.Inline
) {

  /** The title of this document. */
  def title: String = name.display

}

/**
 * Definitions of the components of a document.
 */
object Document {

  /**
   * Base type for inline document components.
   */
  sealed trait Inline {

    /** Returns the text of this inline component with no markup. */
    def strip: String

  }

  /**
   * Simple inline text.
   *
   * @param string The inline text.
   */
  case class Text(string: String) extends Inline {

    /* Return the inline text. */
    override def strip: String = string

  }

  /**
   * Emphasized inline components.
   *
   * @param nested The emphasized inline components.
   */
  case class Emphasis(nested: Inline) extends Inline {

    /* Strip the nested component. */
    override def strip: String = nested.strip

  }

  /**
   * Strong inline components.
   *
   * @param nested The strong inline components.
   */
  case class Strong(nested: Inline) extends Inline {

    /* Strip the nested component. */
    override def strip: String = nested.strip

  }

  sealed trait Link extends Inline {

    /** The title of this link. */
    def title: Option[String]

    /** The content of this link. */
    def nested: Inline

    /* Strip the nested component. */
    final override def strip: String = nested.strip

  }


}
