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

package net.wayfarerx.emanate

/**
 * Base type for all markup elements.
 */
sealed trait Markup {

  /** Returns a text-only version of this markup element. */
  def strip: String

}

/**
 * Definitions of the markup elements.
 */
object Markup {

  //
  // Structural markup items.
  //

  /**
   * Base type for all structural markup elements.
   */
  sealed trait Structure extends Markup

  /**
   * A markup document.
   *
   * @param name        The name of this document.
   * @param description The description of this document.
   * @param author      The optional author of this document.
   * @param content     The main content of this document.
   * @param sections    The subsections of this document.
   */
  case class Document(
    name: Name,
    description: Vector[Inline],
    author: Option[Name],
    content: Vector[Block],
    sections: Vector[Section]
  ) extends Structure {

    /** The title of this document. */
    def title: String = name.display

    /* Return the stripped content. */
    override def strip: String =
      s"#$title\r\n\r\n" + description.map(_.strip).mkString + "\r\n\r\n" +
        author.map("By " + _.display + "\r\n\r\n").getOrElse("") +
        content.map(_.strip).mkString + sections.map(_.strip).mkString

  }

  /**
   * A section of a document.
   *
   * @param level    The level of this section.
   * @param header   The header content of this section.
   * @param content  The main content of this section.
   * @param sections The subsections of this section.
   */
  case class Section(
    level: Int,
    header: Vector[Inline],
    content: Vector[Block],
    sections: Vector[Section]
  ) extends Structure {

    /* Return the stripped content. */
    override def strip: String =
      "#" * level + header.map(_.strip).mkString + "\r\n\r\n" +
        content.map(_.strip).mkString + sections.map(_.strip).mkString

  }

  //
  // Block markup items.
  //

  /**
   * Base type for all block markup elements.
   */
  sealed trait Block extends Markup

  /**
   * A block of inline markup.
   *
   * @param content The inline content.
   */
  case class Paragraph(
    content: Vector[Inline]
  ) extends Block {

    /* Return the stripped content. */
    override def strip: String =
      content.map(_.strip).mkString + "\r\n\r\n"

  }

  /**
   * A code block of inline markup.
   *
   * @param content The inline content.
   */
  case class CodeBlock(
    content: Vector[Inline]
  ) extends Block {

    /* Return the stripped content. */
    override def strip: String =
      content.map(_.strip).mkString + "\r\n\r\n"

  }

  /**
   * A block quote of other blocks.
   *
   * @param content The block content.
   */
  case class BlockQuote(
    content: Vector[Block]
  ) extends Block {

    /* Return the stripped content. */
    override def strip: String =
      content.map(_.strip).mkString

  }

  /**
   * A horizontal rule.
   */
  case object HorizontalRule extends Block {

    /* Return the stripped content. */
    override def strip: String =
      "---\r\n\r\n"

  }

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
    case class Ordered(items: Vector[Item]) extends List {

      /* Return the stripped items. */
      override def strip: String =
        items.zipWithIndex.map(p => s" ${p._2 + 1}. " + p._1.content.map(_.strip).mkString).mkString

    }

    /**
     * An unordered list.
     *
     * @param items The items in this list.
     */
    case class Unordered(items: Vector[Item]) extends List {

      /* Return the stripped items. */
      override def strip: String =
        items.map(" - " + _.content.map(_.strip).mkString).mkString

    }

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
  sealed trait Inline extends Markup

  /**
   * Simple text with no markup.
   *
   * @param content The textual content.
   */
  case class Text(
    content: String
  ) extends Inline {

    /* Return the content. */
    override def strip: String = content

  }

  /**
   * Code text container.
   *
   * @param content The code content.
   */
  case class Code(
    content: Vector[Inline]
  ) extends Inline {

    /* Return the stripped content. */
    override def strip: String =
      content.map(_.strip).mkString

  }

  /**
   * Emphasized text container.
   *
   * @param content The emphasized content.
   */
  case class Emphasized(
    content: Vector[Inline]
  ) extends Inline {

    /* Return the stripped content. */
    override def strip: String =
      content.map(_.strip).mkString

  }

  /**
   * Strong text container.
   *
   * @param content The strong content.
   */
  case class Strong(
    content: Vector[Inline]
  ) extends Inline {

    /* Return the stripped content. */
    override def strip: String =
      content.map(_.strip).mkString

  }

  /**
   * A rendered image.
   *
   * @param asset The image asset.
   * @param title The optional title.
   * @param alt   The optional alt text.
   */
  case class Image(
    asset: Asset[Asset.Image],
    title: Option[String],
    alt: Option[String]
  ) extends Inline {

    /* Return the alt text or title if available. */
    override def strip: String =
      alt orElse title getOrElse ""

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
    final override def strip: String =
      content.map(_.strip).mkString

  }

  /**
   * Definitions of the supported link types.
   */
  object Link {

    /**
     * An Local link to a spot in the current page.
     *
     * @param fragment The fragment to link to.
     * @param title    The optional title.
     * @param content  The content of this link.
     */
    case class Local(
      fragment: String,
      title: Option[String],
      content: Vector[Inline]
    ) extends Link

    /**
     * A link to an entity.
     *
     * @param entity   The entity to link to.
     * @param fragment The optional fragment.
     * @param title    The optional title.
     * @param content  The content of this link.
     */
    case class ToEntity(
      entity: Entity[AnyRef],
      fragment: Option[String],
      title: Option[String],
      content: Vector[Inline]
    ) extends Link

    /**
     * A link to an asset.
     *
     * @param asset   The asset to link to.
     * @param title   The optional title.
     * @param content The content of this link.
     */
    case class ToAsset(
      asset: Asset[Asset.Type],
      title: Option[String],
      content: Vector[Inline]
    ) extends Link

    /**
     * An external link to another site.
     *
     * @param href    The URL to link to.
     * @param title   The optional title.
     * @param content The content of this link.
     */
    case class External(
      href: String,
      title: Option[String],
      content: Vector[Inline]
    ) extends Link

  }

}
