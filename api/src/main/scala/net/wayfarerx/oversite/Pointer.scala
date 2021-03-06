/*
 * Pointer.scala
 *
 * Copyright 2018-2019 wayfarerx <x@wayfarerx.net> (@thewayfarerx)
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

import collection.immutable.SortedSet
import reflect.ClassTag
import cats.data.{NonEmptyList, NonEmptySet}

/**
 * Base type for all pointers.
 *
 * @tparam T The type of data that is pointed to.
 */
sealed trait Pointer[+T <: Pointer.Type] {

  /** The type of this pointer. */
  val tpe: T

  /**
   * Attempts to determine the asset's variant.
   *
   * @return The the asset's variant.
   */
  def variant: Option[Pointer.Asset.Variant]

}

/**
 * Definitions of the supported pointer infrastructure.
 */
object Pointer {

  /** The type of page pointers. */
  type Page = Page.type

  /** The type of image pointers. */
  type Image = Image.type

  /** The type of stylesheet pointers. */
  type Stylesheet = Stylesheet.type

  /** The type of script pointers. */
  type Script = Script.type

  /** The type of JSON pointers. */
  type Json = Json.type

  /** The supported assets. */
  val Assets: List[Asset] = List(Page, Image, Stylesheet, Script, Json)

  /** The index of assets by prefix. */
  lazy val AssetsByPrefix: Map[Name, Asset] =
    Assets.flatMap(a => a.prefix.map(_ -> a)).toMap

  /** The index of asset variants by extension. */
  lazy val VariantsByExtension: Map[Name, Asset.Variant] =
    Assets.flatMap(a => a.variants.toList flatMap (v => v.extensions.toSortedSet map (_ -> v))).toMap

  /**
   * Parses a pointer.
   *
   * @param string The string to parse.
   * @return The parsed pointer.
   */
  def parse(string: String): Pointer[Type] =
    if (isExternalPointer(string)) External.parse(string) else Internal.parse(string)

  /**
   * Returns true if the specified string should be parsed as an external pointer.
   *
   * @param string The string to examine.
   * @return True if the specified string should be parsed as an external pointer.
   */
  private def isExternalPointer(string: String): Boolean =
    string.startsWith("//") || string.contains(":")

  /**
   * Extensions to internal pointers that point to entities.
   *
   * @tparam T The type of entity pointed to by the internal pointer.
   * @param self The internal entity pointer to extend.
   */
  final implicit class EntityExtensions[T <: AnyRef](val self: Pointer[Entity[T]]) extends AnyVal {

    /**
     * Narrows the pointer's entity type.
     *
     * @tparam U The type to narrow to.
     * @return The narrowed entity pointer.
     */
    def narrow[U <: T : ClassTag]: Pointer[Entity[U]] = self match {
      case Search(_, p, q) => Search(Entity[U], p, q)
      case Target(_, p, _) => Target(Entity[U], p, ())
      case _ => sys.error("unreachable")
    }

  }

  /**
   * Base type for internal pointers.
   *
   * @tparam T The type of data that is pointed to.
   */
  sealed trait Internal[+T <: Pointer.Type] extends Pointer[T] {

    /** The prefix of this pointer. */
    def prefix: Prefix

    /**
     * Returns a copy of this pointer with the specified prefix.
     *
     * @param prefix The new prefix to use.
     * @return A copy of this pointer with the specified prefix.
     */
    def withPrefix(prefix: Prefix): Internal[T]

  }

  /**
   * Extractor for internal pointers.
   */
  object Internal {

    /**
     * Parses an internal pointer.
     *
     * @param string The regular string to parse.
     * @return The parsed internal pointer.
     */
    private[Pointer] def parse(string: Path.Regular): Internal[Type] = {
      val (prefix, extra) = Prefix.parse(string)
      extra collect {
        case suffix if suffix contains '.' => Target(Asset.detect(suffix) map (_.asset) getOrElse Page, prefix, suffix)
      } orElse prefix.typed.map { case (p, a) =>
        Search(a, p, extra flatMap (Name(_)) getOrElse a.name)
      }.orElse(extra.flatMap(Name(_)).map(Search(Entity[AnyRef], prefix, _))) getOrElse
        Target(Entity[AnyRef], prefix, ())
    }

    /**
     * Extensions to internal pointers that point to entities.
     *
     * @tparam T The type of entity pointed to by the internal pointer.
     * @param self The internal entity pointer to extend.
     */
    final implicit class EntityExtensions[T <: AnyRef](val self: Internal[Entity[T]]) extends AnyVal {

      /**
       * Narrows the pointer's entity type.
       *
       * @tparam U The type to narrow to.
       * @return The narrowed entity pointer.
       */
      def narrow[U <: T : ClassTag]: Internal[Entity[U]] = self match {
        case Search(_, p, q) => Search(Entity[U], p, q)
        case Target(_, p, _) => Target(Entity[U], p, ())
      }

    }

  }

  /**
   * A pointer that searches for an internal target.
   *
   * @tparam T The type of data that is pointed to.
   * @param tpe    The type of data that is pointed to.
   * @param prefix The scope of this pointer.
   * @param name   The name to search with.
   */
  case class Search[T <: Pointer.Type](
    tpe: T,
    prefix: Prefix,
    name: Name
  ) extends Internal[T] {

    /* No variant available. */
    override def variant: Option[Asset.Variant] = None

    /* Return a new copy. */
    override def withPrefix(prefix: Prefix): Search[T] = copy(prefix = prefix)

  }

  /**
   * Definitions associated with search pointers.
   */
  object Search {

    /**
     * Extensions to internal pointers that point to entities.
     *
     * @tparam T The type of entity pointed to by the internal pointer.
     * @param self The internal entity pointer to extend.
     */
    final implicit class EntityExtensions[T <: AnyRef](val self: Search[Entity[T]]) extends AnyVal {

      /**
       * Narrows the pointer's entity type.
       *
       * @tparam U The type to narrow to.
       * @return The narrowed entity pointer.
       */
      def narrow[U <: T : ClassTag]: Search[Entity[U]] = Search(Entity[U], self.prefix, self.name)

    }

  }

  /**
   * Base class for resolved pointers.
   *
   * @tparam T The type of data that is pointed to.
   */
  sealed trait Resolved[+T <: Pointer.Type] extends Pointer[T] {

    /** The hypertext reference for this pointer. */
    def href: String

    /* No variant available. */
    override def variant: Option[Asset.Variant] =
      Name(href substring href.lastIndexOf('.') + 1) flatMap VariantsByExtension.get

  }

  /**
   * A pointer that directly identifies an internal target.
   *
   * @tparam T The type of data that is pointed to.
   * @tparam S The type of suffix used.
   * @param tpe    The type of data that is pointed to.
   * @param prefix The prefix of this pointer.
   * @param suffix The suffix of this pointer.
   */
  case class Target[T <: Pointer.Type.Aux[S], S](
    tpe: T,
    prefix: Prefix,
    suffix: S
  ) extends Internal[T] with Resolved[T] {

    /* Return a new copy. */
    override def withPrefix(prefix: Prefix): Target[T, S] = copy(prefix = prefix)

    /* Use the type to construct the hypertext reference. */
    override def href: String = tpe.href(prefix, suffix)

  }

  /**
   * Definitions associated with search pointers.
   */
  object Target {

    /**
     * Extensions to internal pointers that point to entities.
     *
     * @tparam T The type of entity pointed to by the internal pointer.
     * @param self The internal entity pointer to extend.
     */
    final implicit class EntityExtensions[T <: AnyRef](val self: Target[Entity[T], Unit]) extends AnyVal {

      /**
       * Narrows the pointer's entity type.
       *
       * @tparam U The type to narrow to.
       * @return The narrowed entity pointer.
       */
      def narrow[U <: T : ClassTag]: Target[Entity[U], Unit] = Target(Entity[U], self.prefix, ())

    }

  }

  /**
   * A pointer to an external resource.
   *
   * @tparam T The type of asset that is pointed to.
   * @param tpe  The type of asset that is pointed to.
   * @param href The hypertext reference of the external resource.
   */
  case class External[+T <: Asset](tpe: T, href: String) extends Resolved[T]

  /**
   * Factory for external pointers.
   */
  object External {

    /**
     * Parses an external pointer.
     *
     * @param string The string to parse.
     * @return The parsed external pointer.
     */
    private[Pointer] def parse(string: String): External[Asset] =
      External(Asset detect string map (_.asset) getOrElse Page, string)

  }

  /**
   * Base type for internal pointer prefixes.
   */
  sealed trait Prefix {

    /** The type of this prefix. */
    type PrefixType >: this.type <: Prefix

    /**
     * Drops the last name in this prefix if it matches any known asset prefix.
     *
     * @return A new prefix if the name was dropped from the end as well as the matching asset.
     */
    def typed: Option[(PrefixType, Asset)]

    /**
     * Converts this prefix into a location as seen from the specified location.
     *
     * @param from The location to resolve this prefix against.
     * @return The location of this prefix if a valid one is found.
     */
    def toLocation(from: Location): Option[Location]

  }

  /**
   * Definitions of the supported prefix types.
   */
  object Prefix {

    /** The empty prefix. */
    val empty = Relative(Path.empty)

    /** The current prefix. */
    val current = Relative(Path(Path.Current))

    /** The root prefix. */
    val root = Absolute(Location.empty)

    /**
     * Returns the prefix that moves from a location to a location.
     *
     * @param from The location to move from.
     * @param to   The location to move to.
     * @return The prefix that moves from a location to a location.
     */
    def apply(from: Location, to: Location): Prefix = {
      val path = from.pathTo(to)
      if (path.elements.length <= to.path.elements.length) Relative(path) else Absolute(to)
    }

    /**
     * Parses a prefix ending in '/' and also returns any meaningful trailing text.
     *
     * @param string The regular string to parse.
     * @return A prefix and any meaningful trailing text.
     */
    private[Pointer] def parse(string: Path.Regular): (Prefix, Option[String]) = {
      val (path, extra) = Path.parse(string)
      (if (string startsWith "/") Absolute(Location.resolved(path)) else Relative(path)) -> extra
    }

    /**
     * A relative prefix.
     *
     * @param path The relative path.
     */
    case class Relative(path: Path) extends Prefix {

      /* Set the prefix type. */
      override type PrefixType = Relative

      /* Drop the last name if it matches an asset prefix. */
      override def typed: Option[(PrefixType, Asset)] =
        path.elements match {
          case init :+ Path.Child(n) => AssetsByPrefix.get(n) map (Relative(Path(init)) -> _)
          case _ => None
        }

      /* Resolve the path against the location. */
      override def toLocation(from: Location): Option[Location] = from ++ path

      /* Return the path's string. */
      override def toString: String = path.toString

    }

    /**
     * An absolute prefix.
     *
     * @param location The absolute location.
     */
    case class Absolute(location: Location) extends Prefix {

      /* Set the prefix type. */
      override type PrefixType = Absolute

      /* Drop the last name if it matches an asset prefix. */
      override def typed: Option[(PrefixType, Asset)] =
        location.path.elements match {
          case init :+ Path.Child(n) => AssetsByPrefix.get(n) map (Absolute(Location.resolved(Path(init))) -> _)
          case _ => None
        }

      /* Return the location. */
      override def toLocation(from: Location): Option[Location] = Some(location)

      /* Return the location's string. */
      override def toString: String = location.toString

    }

  }

  /**
   * Base type for all pointer types.
   */
  sealed trait Type {

    /** The suffix type of this pointer type. */
    type SuffixType

    /** The type of this pointer. */
    type PointerType >: Pointer[this.type] <: Pointer[Type]

    /**
     * Generate a hypertext reference from a prefix and a target.
     *
     * @param prefix The prefix of the hypertext reference.
     * @param suffix The suffix of the hypertext reference.
     * @return A hypertext reference from a prefix and a suffix.
     */
    def href(prefix: Prefix, suffix: SuffixType): String

  }

  /**
   * Definitions associated with pointer types.
   */
  object Type {

    /** Mapping of a type's dependencies into generic space. */
    type Aux[T] = Type {type SuffixType = T}

  }

  /**
   * The type of entity pointers.
   *
   * @param classInfo The information about the entity class.
   */
  case class Entity[T <: AnyRef](classInfo: Class[_]) extends Type {

    /* Define the file name type. */
    override type SuffixType = Unit

    /* Define the pointer type. */
    override type PointerType = Pointer[Entity[T]]

    /**
     * Creates a pointer that searches the current location for an entity.
     *
     * @param name The name to search for.
     * @return A pointer that searches the current location for an entity.
     */
    def apply(name: Name): Search[Entity[T]] =
      apply(Prefix.empty, name)

    /**
     * Creates a pointer that searches the specified path for an entity.
     *
     * @param path The path to search from.
     * @param name The name to search for.
     * @return A pointer that searches the specified path for an entity.
     */
    def apply(path: Path, name: Name): Search[Entity[T]] =
      apply(Prefix.Relative(path), name)

    /**
     * Creates a pointer that searches the specified location for an entity.
     *
     * @param location The location to search from.
     * @param name     The name to search for.
     * @return A pointer that searches the specified location for an entity.
     */
    def apply(location: Location, name: Name): Search[Entity[T]] =
      apply(Prefix.Absolute(location), name)

    /**
     * Creates a pointer that searches the specified path for an entity.
     *
     * @param prefix The prefix to search from.
     * @param name   The name to search for.
     * @return A pointer that searches the specified path for an entity.
     */
    def apply(prefix: Prefix, name: Name): Search[Entity[T]] =
      Search(this, prefix, name)

    /**
     * Creates a pointer that searches the specified path for an entity.
     *
     * @param path The path of the entity.
     * @return A pointer that searches the specified path for an entity.
     */
    def apply(path: Path): Target[Entity[T], SuffixType] =
      apply(Prefix.Relative(path))

    /**
     * Creates a pointer that searches the specified location for an entity.
     *
     * @param location The location of the entity.
     * @return A pointer that searches the specified location for an entity.
     */
    def apply(location: Location): Target[Entity[T], SuffixType] =
      apply(Prefix.Absolute(location))

    /**
     * Creates a pointer that searches the specified location for an entity.
     *
     * @param prefix The prefix of the entity.
     * @return A pointer that searches the specified location for an entity.
     */
    def apply(prefix: Prefix): Target[Entity[T], SuffixType] =
      Target(this, prefix, ())

    /**
     * Creates a pointer to an entity by parsing the specified path.
     *
     * @param string The regular string to parse an entity pointer from.
     * @return A pointer to an entity created by parsing the specified path.
     */
    def parse(string: Path.Regular): Internal[Entity[T]] = {
      val (prefix, name) = Prefix.parse(string)
      name flatMap (Name(_)) map (Search(this, prefix, _)) getOrElse Target(this, prefix, ())
    }

    /* Only use the prefix for entity hypertext references. */
    override def href(prefix: Prefix, suffix: SuffixType): String =
      (if (prefix == Prefix.empty) Prefix.current else prefix).toString

  }

  /**
   * Factory for entity types.
   */
  object Entity {

    /**
     * Creates a new entity type.
     *
     * @tparam T The type of the underlying entity.
     * @return A new entity type.
     */
    def apply[T <: AnyRef : ClassTag]: Entity[T] =
      new Entity[T](implicitly[ClassTag[T]].runtimeClass)

  }

  /**
   * The base type for asset pointers.
   */
  sealed trait Asset extends Type {
    self =>

    /** The type of this asset. */
    type AssetType >: this.type <: Asset

    /* Define the pointer type. */
    final override type PointerType = Pointer[AssetType]

    /* Define the suffix type. */
    final override type SuffixType = String

    /** The prefix that is prepended to the asset name when searching. */
    def prefix: Option[Name]

    /** The default name of this type of asset. */
    def name: Name

    /** The supported variants of this asset type. */
    def variants: NonEmptyList[Asset.Variant]

    /**
     * Creates a pointer that searches the current location for an asset.
     *
     * @param name The name to search for.
     * @return A pointer that searches the current location for an asset.
     */
    final def apply(name: Name): Search[AssetType] =
      apply(Prefix.empty, name)

    /**
     * Creates a pointer that searches the specified path for an asset.
     *
     * @param path The path to search from.
     * @param name The name to search for.
     * @return A pointer that searches the specified path for an asset.
     */
    final def apply(path: Path, name: Name): Search[AssetType] =
      apply(Prefix.Relative(path), name)

    /**
     * Creates a pointer that searches the specified location for an asset.
     *
     * @param location The location to search from.
     * @param name     The name to search for.
     * @return A pointer that searches the specified location for an asset.
     */
    final def apply(location: Location, name: Name): Search[AssetType] =
      apply(Prefix.Absolute(location), name)

    /**
     * Creates a pointer that searches the specified location for an asset.
     *
     * @param prefix The prefix to search from.
     * @param name   The name to search for.
     * @return A pointer that searches the specified location for an asset.
     */
    final def apply(prefix: Prefix, name: Name): Search[AssetType] =
      Search(this, prefix, name)

    /**
     * Creates a pointer that searches the current location for an entity.
     *
     * @param suffix The suffix of the pointer.
     * @return A pointer that searches the current location for an entity.
     */
    final def apply(suffix: SuffixType): Target[AssetType, SuffixType] =
      apply(Prefix.empty, suffix)

    /**
     * Creates a pointer that searches the specified path for an entity.
     *
     * @param prefix The path of the pointer.
     * @param suffix The suffix of the pointer.
     * @return A pointer that searches the specified path for an entity.
     */
    final def apply(prefix: Path, suffix: SuffixType): Target[AssetType, SuffixType] =
      apply(Prefix.Relative(prefix), suffix)

    /**
     * Creates a pointer that searches the specified location for an entity.
     *
     * @param prefix The location of the pointer.
     * @param suffix The suffix of the pointer.
     * @return A pointer that searches the specified location for an entity.
     */
    final def apply(prefix: Location, suffix: SuffixType): Target[AssetType, SuffixType] =
      apply(Prefix.Absolute(prefix), suffix)

    /**
     * Creates a pointer that searches the specified location for an entity.
     *
     * @param prefix The prefix of the pointer.
     * @param suffix The suffix of the pointer.
     * @return A pointer that searches the specified location for an entity.
     */
    final def apply(prefix: Prefix, suffix: SuffixType): Target[AssetType, SuffixType] =
      Target(this, prefix, suffix)

    /**
     * Creates a pointer to an asset by parsing the specified path.
     *
     * @param string The string to parse an asset pointer from.
     * @return A pointer to an asset created by parsing the specified path.
     */
    final def parse(string: String): Pointer[AssetType] =
      if (isExternalPointer(string)) External[AssetType](this, string) else {
        val (prefix, suffix) = Prefix.parse(string)
        suffix flatMap {
          case s if s contains '.' => Some(Target[AssetType, SuffixType](this, prefix, s))
          case s => Name(s) map (Search[AssetType](this, prefix, _))
        } getOrElse Search[AssetType](this, prefix, name)
      }

    /* Concatenate the prefix and suffix for asset hypertext references. */
    final override def href(prefix: Prefix, suffix: SuffixType): String =
      prefix + suffix

  }

  /**
   * Extractor for pointer asset types.
   */
  object Asset {

    /**
     * Detects the type of an asset from the extension.
     *
     * @param string The string to detect the asset type from.
     * @return The detected asset type.
     */
    final private[Pointer] def detect(string: String): Option[Variant] = {
      val file = string lastIndexOf '/' match {
        case i if i >= 0 => string substring i + 1
        case _ => string
      }
      Name(file lastIndexOf "." match {
        case i if i >= 0 => file substring i + 1
        case _ => file
      }) flatMap VariantsByExtension.get
    }

    /**
     * A particular variant of the enclosing asset type.
     *
     * @param asset      The asset this variant represents.
     * @param mimeType   The mime type of this variant.
     * @param extensions The extensions for this variant.
     */
    case class Variant(asset: Asset, mimeType: String, extensions: NonEmptySet[Name]) {

      /** The default extension for this variant. */
      def extension: Name = extensions.toSortedSet minBy (_.normal.length)

    }

    /**
     * Factory for asset variants.
     */
    object Variant {

      /**
       * Creates an asset variant with the specified settings.
       *
       * @param asset      The asset that this variant represents.
       * @param mimeType   The mime type of the variant.
       * @param extension  The default extension to use for the asset variant.
       * @param extensions The other extensions to use for the asset variant.
       * @return An asset variant with the specified settings.
       */
      def apply(asset: Asset, mimeType: String, extension: Name, extensions: Name*): Variant =
        Variant(asset, mimeType, NonEmptySet(extension, SortedSet(extensions: _*)))

      /**
       * Creates an asset variant with the specified settings.
       *
       * @param asset      The asset that this variant represents.
       * @param mimeType   The mime type of the variant.
       * @param extension  The default extension to use for the asset variant.
       * @param extensions The other extensions to use for the asset variant.
       * @return An asset variant with the specified settings.
       */
      def of(asset: Asset, mimeType: String, extension: Name, extensions: SortedSet[Name]): Variant =
        apply(asset, mimeType, extension, extensions.toSeq: _*)

      /**
       * Creates an asset variant with a single extension.
       *
       * @param asset     The asset that this variant represents.
       * @param mimeType  The mime type of the variant.
       * @param extension The extension to use for the asset variant.
       * @return An asset variant with a single extension.
       */
      def one(asset: Asset, mimeType: String, extension: Name): Variant =
        fromSet(asset, mimeType, SortedSet(extension)).get

      /**
       * Attempts to crate an asset variant with the specified settings.
       *
       * @param asset      The asset that this variant represents.
       * @param mimeType   The mime type of the variant.
       * @param extensions The extensions to use for the asset variant.
       * @return An asset variant with the specified settings if one could be created.
       */
      def fromSet(asset: Asset, mimeType: String, extensions: SortedSet[Name]): Option[Variant] =
        NonEmptySet.fromSet(extensions) map (Variant(asset, mimeType, _))

    }

  }

  /**
   * The HTML page pointer type.
   */
  case object Page extends Asset {

    /** HTML pages. */
    val html: Asset.Variant = Asset.Variant.one(this, "text/html", name"html")

    /* Define the asset type. */
    override type AssetType = Page

    /* The search prefix. */
    override val prefix: Option[Name] = None

    /* The default name. */
    override val name: Name = name"page"

    /* The extensions to search for. */
    override val variants: NonEmptyList[Asset.Variant] = NonEmptyList.of(html)

  }

  /**
   * The raster image pointer type.
   */
  case object Image extends Asset {

    /** GIF images. */
    val gif: Asset.Variant = Asset.Variant.one(this, "image/gif", name"gif")

    /** JPG images. */
    val jpg: Asset.Variant = Asset.Variant(this, "image/jpeg", name"jpg", name"jpeg")

    /** PNG images. */
    val png: Asset.Variant = Asset.Variant.one(this, "image/png", name"png")

    /** ICO images. */
    val ico: Asset.Variant = Asset.Variant.one(this, "image/vnd.microsoft.icon", name"ico")

    /** SVG images. */
    val svg: Asset.Variant = Asset.Variant.one(this, "image/svg+xml", name"svg")

    /* Define the asset type. */
    override type AssetType = Image

    /* The search prefix. */
    override val prefix: Option[Name] = Some(name"images")

    /* The default name. */
    override val name: Name = name"image"

    /* The extensions to search for. */
    override val variants: NonEmptyList[Asset.Variant] = NonEmptyList.of(gif, jpg, png)

  }

  /**
   * The CSS stylesheet pointer type.
   */
  case object Stylesheet extends Asset {

    /** CSS stylesheets. */
    val css: Asset.Variant = Asset.Variant.one(this, "text/css", name"css")

    /* Define the asset type. */
    override type AssetType = Stylesheet

    /* The search prefix. */
    override val prefix: Option[Name] = Some(name"stylesheets")

    /* The default name. */
    override val name: Name = name"stylesheet"

    /* The extensions to search for. */
    override val variants: NonEmptyList[Asset.Variant] = NonEmptyList.of(css)

  }

  /**
   * The JavaScript pointer type.
   */
  case object Script extends Asset {

    /** JS scripts. */
    val js: Asset.Variant = Asset.Variant.one(this, "text/javascript", name"js")

    /* Define the asset type. */
    override type AssetType = Script

    /* The search prefix. */
    override val prefix: Option[Name] = Some(name"scripts")

    /* The default name. */
    override val name: Name = name"script"

    /* The extensions to search for. */
    override val variants: NonEmptyList[Asset.Variant] = NonEmptyList.of(js)

  }

  /**
   * The JSON pointer type.
   */
  case object Json extends Asset {

    /** JS scripts. */
    val json: Asset.Variant = Asset.Variant.one(this, "application/json", name"json")

    /* Define the asset type. */
    override type AssetType = Json

    /* The search prefix. */
    override val prefix: Option[Name] = Some(name"data")

    /* The default name. */
    override val name: Name = name"data"

    /* The extensions to search for. */
    override val variants: NonEmptyList[Asset.Variant] = NonEmptyList.of(json)

  }

}
