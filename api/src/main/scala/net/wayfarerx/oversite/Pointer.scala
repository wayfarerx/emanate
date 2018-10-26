/*
 * Pointer.scala
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

import collection.immutable.ListSet
import reflect.ClassTag

/**
 * Base type for all pointers.
 *
 * @tparam T The type of data that is pointed to.
 */
sealed trait Pointer[+T <: Pointer.Type] {

  /** The type of this pointer. */
  val tpe: T

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

  /** The supported assets. */
  val Assets: Vector[Asset] = Vector(Page, Image, Stylesheet, Script)

  /** The index of assets by prefix. */
  private val assetsByPrefix = Assets.map(a => a.prefix -> a).toMap

  /** The index of assets by extension. */
  private val assetsByExtension = Assets.flatMap(a => a.extensions map (_ -> a)).toMap

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
      case _ => sys.error("unreachable code")
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
        case suffix if suffix contains '.' => Target(Asset.detect(suffix) getOrElse Page, prefix, suffix)
      } orElse {
        prefix.typed map { case (p, a) =>
          Search(a, p, extra flatMap (Name(_)) getOrElse a.default)
        } orElse extra.flatMap(Name(_)).map(Search(Entity[AnyRef], prefix, _))
      } getOrElse Target(Entity[AnyRef], prefix, ())
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
   * @param prefix The prefix of this pointer.
   * @param query  The query to search with.
   */
  case class Search[T <: Pointer.Type](
    tpe: T,
    prefix: Prefix,
    query: Name
  ) extends Internal[T] {

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
      def narrow[U <: T : ClassTag]: Search[Entity[U]] = Search(Entity[U], self.prefix, self.query)

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
      External(Asset detect string getOrElse Page, string)

  }

  /**
   * Base type for internal pointer prefixes.
   */
  sealed trait Prefix {

    /** The type of this prefix. */
    type PrefixType >: this.type <: Prefix

    /**
     * Drops the last name in this prefix if it matches any known asset name.
     *
     * @return A new prefix if the name was dropped from the end as well as the matching asset.
     */
    def typed: Option[(PrefixType, Asset)]

  }

  /**
   * Definitions of the supported prefix types.
   */
  object Prefix {

    /** The empty prefix. */
    val empty = Relative(Path.empty)

    /** The current prefix. */
    val current = Relative(Path(Vector(Path.Current)))

    /** The root prefix. */
    val root = Absolute(Location.empty)

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
          case init :+ Path.Child(n) => assetsByPrefix.get(n) map (Relative(Path(init)) -> _)
          case _ => None
        }

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
          case init :+ Path.Child(n) => assetsByPrefix.get(n) map (Absolute(Location.resolved(Path(init))) -> _)
          case _ => None
        }

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
  case class Entity[T <: AnyRef] private(classInfo: Class[_]) extends Type {

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
      Entity[T](implicitly[ClassTag[T]].runtimeClass)

  }

  /**
   * The base type for asset pointers.
   */
  sealed trait Asset extends Type {

    /** The type of this asset. */
    type AssetType >: this.type <: Asset

    /* Define the pointer type. */
    final override type PointerType = Pointer[AssetType]

    /* Define the suffix type. */
    final override type SuffixType = String

    /** The prefix that is prepended to the asset name when searching. */
    def prefix: Name

    /** The default name of this type of asset. */
    def default: Name

    /** The extensions that are appended to the asset name when searching. */
    def extensions: ListSet[String]

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
        } getOrElse Search[AssetType](this, prefix, default)
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
    final private[Pointer] def detect(string: String): Option[Asset] = {
      val substring = string substring string.lastIndexOf('/') + 1
      assetsByExtension get substring.substring(substring.lastIndexOf('.') + 1)
    }

  }

  /**
   * The HTML page pointer type.
   */
  case object Page extends Asset {

    /* Define the asset type. */
    override type AssetType = Page

    /* The search prefix. */
    override val prefix: Name = name"pages"

    /* The default name. */
    override val default: Name = name"page"

    /* The extensions to search for. */
    override val extensions: ListSet[String] = ListSet("html")

  }

  /**
   * The raster image pointer type.
   */
  case object Image extends Asset {

    /* Define the asset type. */
    override type AssetType = Image

    /* The search prefix. */
    override val prefix: Name = name"images"

    /* The default name. */
    override val default: Name = name"image"

    /* The extensions to search for. */
    override val extensions: ListSet[String] = ListSet("jpg", "jpeg", "gif", "png")

  }

  /**
   * The CSS stylesheet pointer type.
   */
  case object Stylesheet extends Asset {

    /* Define the asset type. */
    override type AssetType = Stylesheet

    /* The search prefix. */
    override val prefix: Name = name"stylesheets"

    /* The default name. */
    override val default: Name = name"stylesheet"

    /* The extensions to search for. */
    override val extensions: ListSet[String] = ListSet("css")

  }

  /**
   * The JavaScript pointer type.
   */
  case object Script extends Asset {

    /* Define the asset type. */
    override type AssetType = Script

    /* The search prefix. */
    override val prefix: Name = name"scripts"

    /* The default name. */
    override val default: Name = name"script"

    /* The extensions to search for. */
    override val extensions: ListSet[String] = ListSet("js")

  }

}
