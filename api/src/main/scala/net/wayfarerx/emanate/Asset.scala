/*
 * Asset.scala
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

import scala.collection.immutable.ListSet

/**
 * Base class for references to assets.
 *
 * @tparam T The type of asset.
 */
sealed trait Asset[+T <: Asset.Type] {

  /** The type of asset. */
  def assetType: T

}

/**
 * Factory for assets.
 */
object Asset {

  /** The type of image assets. */
  type Image = Image.type

  /** The type of stylesheet assets. */
  type Stylesheet = Stylesheet.type

  /** The type of script assets. */
  type Script = Script.type

  /**
   * A reference to an asset by name.
   *
   * @tparam T The type of asset.
   * @param name The name of the asset.
   * @param assetType The type of the asset.
   */
  case class Named[T <: Asset.Type](name: Name, assetType: T) extends Asset[T]

  /**
   * Base class for resolved references to assets.
   *
   * @tparam T The type of asset.
   */
  sealed trait Resolved[T <: Asset.Type] extends Asset[T]

  /**
   * A reference to an asset by relative path.
   *
   * @tparam T The type of asset.
   * @param path The path to the desired asset.
   * @param assetType The type of the asset.
   */
  case class Relative[T <: Asset.Type](path: Path, fileName: String, assetType: T) extends Resolved[T]

  /**
   * A reference to an asset by absolute path.
   *
   * @tparam T The type of asset.
   * @param location The location of the desired asset.
   * @param assetType The type of the asset.
   */
  case class Absolute[T <: Asset.Type](location: Location, fileName: String, assetType: T) extends Resolved[T]

  /**
   * Base type for all asset types.
   */
  trait Type {

    /** The asset type of this type. */
    type FileType <: Asset.Type

    /** The name of this type. */
    def name: Name

    /** The prefix that is prepended to the asset name when searching. */
    def prefix: String

    /** The extensions that satisfy this asset type. */
    def extensions: ListSet[String]

    /**
     * Creates a new asset of this type.
     *
     * @param name The name of the asset.
     * @return A new asset of this type.
     */
    def apply(name: Name): Asset.Named[FileType]

    /**
     * Creates a new asset of this type.
     *
     * @param path     The path of the asset.
     * @param fileName The name of the asset file.
     * @return A new asset of this type.
     */
    def apply(path: Path, fileName: String): Asset.Relative[FileType]

    /**
     * Creates a new asset of this type.
     *
     * @param location The location of the asset.
     * @param fileName The name of the asset file.
     * @return A new asset of this type.
     */
    def apply(location: Location, fileName: String): Asset.Absolute[FileType]

    /**
     * Attempts to create a new asset of this type.
     *
     * @param string The string to parse.
     * @return A new asset of this type if one could be created.
     */
    final def apply(string: String): Option[Asset[FileType]] = {

      def split: (Path, String) = string lastIndexOf '/' match {
        case index if index > 0 => Path(string.substring(0, index)) -> string.substring(index + 1)
        case _ => Path.empty -> string
      }

      string match {
        case absolute if absolute startsWith "/" =>
          val (path, fileName) = split
          Location(path) map (apply(_, fileName))
        case relative if relative exists (c => c == '/' || c == '.') =>
          val (path, fileName) = split
          Some(apply(path, fileName))
        case named =>
          Name(named) map apply
      }
    }

  }

  /**
   * A raster image asset.
   */
  case object Image extends Type {

    /* The asset type. */
    override type FileType = Image

    /* The name of this type. */
    override val name: Name = Name("image").get

    /* The search prefix. */
    override val prefix: String = "images"

    /* The extensions to search for. */
    override val extensions: ListSet[String] = ListSet("jpg", "jpeg", "gif", "png")

    /* Create an asset. */
    override def apply(name: Name): Asset.Named[Image] =
      Asset.Named(name, this)

    /* Create an asset. */
    override def apply(path: Path, fileName: String): Asset.Relative[Image] =
      Asset.Relative(path, fileName, this)

    /* Create an asset. */
    override def apply(location: Location, fileName: String): Asset.Absolute[Image] =
      Asset.Absolute(location, fileName, this)

  }

  /**
   * A CSS stylesheet asset.
   */
  case object Stylesheet extends Type {

    /* The asset type. */
    override type FileType = Stylesheet

    /* The name of this type. */
    override val name: Name = Name("stylesheet").get

    /* The search prefix. */
    override val prefix: String = "css"

    /* The extensions to search for. */
    override val extensions: ListSet[String] = ListSet("css")

    /* Create an asset. */
    override def apply(name: Name): Asset.Named[Stylesheet] =
      Asset.Named(name, this)

    /* Create an asset. */
    override def apply(path: Path, fileName: String): Asset.Relative[Stylesheet] =
      Asset.Relative(path, fileName, this)

    /* Create an asset. */
    override def apply(location: Location, fileName: String): Asset.Absolute[Stylesheet] =
      Asset.Absolute(location, fileName, this)

  }

  /**
   * A JavaScript asset.
   */
  case object Script extends Type {

    /* The asset type. */
    override type FileType = Script

    /* The name of this type. */
    override val name: Name = Name("script").get

    /* The search prefix. */
    override val prefix: String = "js"

    /* The extensions to search for. */
    override val extensions: ListSet[String] = ListSet("js")

    /* Create an asset. */
    override def apply(name: Name): Asset.Named[Script] =
      Asset.Named(name, this)

    /* Create an asset. */
    override def apply(path: Path, fileName: String): Asset.Relative[Script] =
      Asset.Relative(path, fileName, this)

    /* Create an asset. */
    override def apply(location: Location, fileName: String): Asset.Absolute[Script] =
      Asset.Absolute(location, fileName, this)

  }

}
