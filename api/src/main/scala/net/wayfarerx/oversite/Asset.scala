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

package net.wayfarerx.oversite

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
   * Attempts to create a new asset from the specified string.
   *
   * @param string The string to parse.
   * @param types  The registered asset types.
   * @return A new asset if one could be created.
   */
  def apply(string: String)(implicit types: Types): Option[Asset[Type]] =
    string indexOf ':' match {
      case index if index > 0 =>
        Name(string take index) flatMap (types(_)) flatMap (_ (string drop index + 1))
      case _ =>
        string lastIndexOf '.' match {
          case index if index > 0 =>
            Some(string substring index + 1) filterNot (_.isEmpty) flatMap (types(_)) flatMap (_ (string))
          case _ =>
            None
        }
    }

  /**
   * A reference to an asset by name.
   *
   * @tparam T The type of asset.
   * @param name      The name of the asset.
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
   * @param path      The path to the desired asset.
   * @param assetType The type of the asset.
   */
  case class Relative[T <: Asset.Type](path: Path, fileName: String, assetType: T) extends Resolved[T]

  /**
   * A reference to an asset by absolute path.
   *
   * @tparam T The type of asset.
   * @param location  The location of the desired asset.
   * @param assetType The type of the asset.
   */
  case class Absolute[T <: Asset.Type](location: Location, fileName: String, assetType: T) extends Resolved[T]

  /**
   * Base type for all asset types.
   */
  trait Type {

    /** The asset type of this type. */
    type AssetType >: this.type <: Asset.Type

    /** The name of this type. */
    def name: Name = simpleName.get

    /** The prefix that is prepended to the asset name when searching. */
    def prefix: String

    /** The extensions that satisfy this asset type. */
    def extensions: ListSet[String]

    /** The simple name of this type. */
    private lazy val simpleName: Option[Name] = Name(getClass.getSimpleName)

    /**
     * Creates a new asset of this type.
     *
     * @param assetName The name of the asset.
     * @return A new asset of this type.
     */
    final def apply(assetName: Name): Asset.Named[AssetType] =
      Asset.Named(assetName, this)

    /**
     * Creates a new asset of this type.
     *
     * @param assetPath     The path of the asset.
     * @param assetFileName The name of the asset file.
     * @return A new asset of this type.
     */
    final def apply(assetPath: Path, assetFileName: String): Asset.Relative[AssetType] =
      Asset.Relative(assetPath, assetFileName, this)

    /**
     * Creates a new asset of this type.
     *
     * @param assetLocation The location of the asset.
     * @param assetFileName The name of the asset file.
     * @return A new asset of this type.
     */
    final def apply(assetLocation: Location, assetFileName: String): Asset.Absolute[AssetType] =
      Asset.Absolute(assetLocation, assetFileName, this)

    /**
     * Attempts to create a new asset of this type.
     *
     * @param string The string to parse.
     * @return A new asset of this type if one could be created.
     */
    final def apply(string: String): Option[Asset[AssetType]] = {

      def pathAndFileName: (Path, String) = string lastIndexOf '/' match {
        case index if index > 0 => Path(string.substring(0, index)) -> string.substring(index + 1)
        case _ => Path.empty -> string
      }

      string match {
        case absolute if absolute startsWith "/" =>
          val (path, fileName) = pathAndFileName
          Location(path) map (apply(_, fileName))
        case relative if relative exists (c => c == '/' || c == '.') =>
          val (path, fileName) = pathAndFileName
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
    override type AssetType = Image

    /* The search prefix. */
    override val prefix: String = "images"

    /* The extensions to search for. */
    override val extensions: ListSet[String] = ListSet("jpg", "jpeg", "gif", "png")

  }

  /**
   * A CSS stylesheet asset.
   */
  case object Stylesheet extends Type {

    /* The asset type. */
    override type AssetType = Stylesheet

    /* The search prefix. */
    override val prefix: String = "css"

    /* The extensions to search for. */
    override val extensions: ListSet[String] = ListSet("css")

  }

  /**
   * A JavaScript asset.
   */
  case object Script extends Type {

    /* The asset type. */
    override type AssetType = Script

    /* The search prefix. */
    override val prefix: String = "js"

    /* The extensions to search for. */
    override val extensions: ListSet[String] = ListSet("js")

  }

  /**
   * An index of types by their names and the extensions they support.
   *
   * @param typesByName      The index of types by their names.
   * @param typesByExtension The index of types by the extensions they support.
   */
  case class Types private(
    types: ListSet[Type],
    typesByName: Map[Name, Type],
    typesByExtension: Map[String, Type]
  ) {

    /**
     * Returns the type for the specified name if one is registered.
     *
     * @param name The name to return the type for.
     * @return The type for the specified name if one is registered.
     */
    def apply(name: Name): Option[Type] =
      typesByName get name

    /**
     * Returns the type for the specified extension if one is registered.
     *
     * @param extension The extension to return the type for.
     * @return The type for the specified extension if one is registered.
     */
    def apply(extension: String): Option[Type] =
      typesByExtension get extension

    /**
     * Includes the specified type in this index.
     *
     * @param that The type to include in this index.
     * @return A new index that includes the specified type.
     */
    def :+(that: Type): Types = {
      val newTypes = types + that
      if (newTypes.size == types.size) this
      else copy(
        newTypes,
        if (typesByName contains that.name) typesByName else typesByName + (that.name -> that),
        typesByExtension ++ (that.extensions filterNot typesByExtension.contains map (_ -> that))
      )
    }

    /**
     * Includes the specified types in this index.
     *
     * @param that The types to include in this index.
     * @return A new index that includes the specified types.
     */
    def ++(that: Types): Types =
      (this /: that.types) (_ :+ _)

  }

  /**
   * Factory for type indexes.
   */
  object Types {

    /** The empty collection of asset types. */
    val empty: Types = Types(ListSet.empty[Type], Map.empty[Name, Type], Map.empty[String, Type])

    /** The default collection of asset types. */
    val default: Types = Types(Image, Stylesheet, Script)

    /**
     * Creates a new index that includes the specified types.
     *
     * @param types The types to include in the index.
     * @return The new index.
     */
    def apply(types: Type*): Types =
      (empty /: types) (_ :+ _)

  }

}
