/*
 * Markup.scala
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
 * Base type for all markup elements.
 */
sealed trait Markup

/**
 * Definitions of the markup elements.
 */
object Markup {

  //
  // Block markup items.
  //

  /**
   * Base type for all block markup elements.
   */
  sealed trait Block extends Markup

  /**
   * A horizontal rule.
   */
  case object HorizontalRule extends Block

  /**
   * A block of inline markup.
   *
   * @param content The inline content.
   */
  case class Paragraph(content: Vector[Inline]) extends Block

  /**
   * A code block of inline markup.
   *
   * @param content The inline content.
   */
  case class CodeBlock(content: Vector[Inline]) extends Block

  /**
   * A block quote of other blocks.
   *
   * @param content The block content.
   */
  case class BlockQuote(content: Vector[Block]) extends Block

  /**
   * Base type for list markup.
   */
  sealed trait List extends Block {

    /** The items in this list. */
    def items: Vector[List.Item]

  }

  /**
   * Definitions of the supported list types.
   */
  object List {

    /**
     * An ordered list.
     *
     * @param items The items in this list.
     */
    case class Ordered(items: Vector[Item]) extends List

    /**
     * An unordered list.
     *
     * @param items The items in this list.
     */
    case class Unordered(items: Vector[Item]) extends List

    /**
     * A single item in a list.
     *
     * @param content The content of this item.
     */
    case class Item(content: Vector[Block])

  }

  //
  // Inline markup items.
  //

  /**
   * Base type for all inline markup elements.
   */
  sealed trait Inline extends Markup {

    /** Returns a text-only version of this markup element. */
    def strip: String

  }

  /**
   * Simple text with no markup.
   *
   * @param content The textual content.
   */
  case class Text(content: String) extends Inline {

    /* Return the content. */
    override def strip: String = content

  }

  /**
   * Code text container.
   *
   * @param content The code content.
   */
  case class Code(content: Vector[Inline]) extends Inline {

    /* Return the stripped content. */
    override def strip: String = content.map(_.strip).mkString

  }

  /**
   * Emphasized text container.
   *
   * @param content The emphasized content.
   */
  case class Emphasized(content: Vector[Inline]) extends Inline {

    /* Return the stripped content. */
    override def strip: String = content.map(_.strip).mkString

  }

  /**
   * Strong text container.
   *
   * @param content The strong content.
   */
  case class Strong(content: Vector[Inline]) extends Inline {

    /* Return the stripped content. */
    override def strip: String = content.map(_.strip).mkString

  }

  /**
   * A rendered image.
   *
   * @param pointer The image asset.
   * @param title   The optional title.
   * @param alt     The optional alt text.
   */
  case class Image(pointer: Pointer[Pointer.Image], title: Option[String], alt: Option[String]) extends Inline {

    /* Return the alt text or title if available. */
    override def strip: String = alt orElse title getOrElse ""

  }

  /**
   * Base type for links.
   */
  sealed trait Link extends Inline {

    /** The optional title. */
    def title: Option[String]

    /** The content of this link. */
    def content: Vector[Inline]

    /* Return the stripped content. */
    final override def strip: String = content.map(_.strip).mkString

  }

  /**
   * Definitions of the supported link types.
   */
  object Link {

    /**
     * An link that jumps to a spot in the current page.
     *
     * @param fragment The fragment to link to.
     * @param title    The optional title.
     * @param content  The content of this link.
     */
    case class Jump(fragment: String, title: Option[String], content: Vector[Inline]) extends Link

    /**
     * A link that loads a different resource.
     *
     * @param pointer The pointer to link to.
     * @param title   The optional title.
     * @param content The content of this link.
     */
    case class Load(pointer: Pointer[Pointer.Type], title: Option[String], content: Vector[Inline]) extends Link

    /**
     * A link that loads a different resource.
     *
     * @param pointer  The pointer to link to.
     * @param fragment The fragment to link to.
     * @param title    The optional title.
     * @param content  The content of this link.
     */
    case class LoadAndJump(
      pointer: Pointer[Pointer.Type],
      fragment: String,
      title: Option[String],
      content: Vector[Inline]
    ) extends Link

  }

}
