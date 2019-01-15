/*
 * Path.scala
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

import language.implicitConversions

/**
 * A path within the scope of a site.
 *
 * @param elements The elements that comprise this path.
 */
case class Path(elements: Vector[Path.Element]) {

  import Path._

  /** The parent of this path. */
  def parent: Path = this :+ Parent

  /** True if this path is normal (has no '.' elements and no unresolved '..' elements). */
  def isNormalized: Boolean =
    !elements.contains(Current) && !elements.dropWhile(_ == Parent).contains(Parent)

  /** The normalized version of this path. */
  def normalized: Path =
    if (isNormalized) this
    else Path((Vector.empty[Element] /: elements) { (result, element) =>
      element match {
        case Current => result
        case Parent if result.isEmpty || result.last == Parent => result :+ Parent
        case Parent => result.init
        case child@Child(_) => result :+ child
      }
    })

  /** True if this path is resolved (has no '.' or '..' elements). */
  def isResolved: Boolean =
    !elements.exists(e => e == Current || e == Parent)

  /** The resolved version of this path. */
  def resolved: Path =
    if (isResolved) this else Path(normalized.elements dropWhile (_ == Path.Parent))

  /**
   * Prepends a child element with the specified name to this path.
   *
   * @param name The name of the child element to prepend.
   * @return This path with a named child prepended.
   */
  def +:(name: Name): Path = Child(name) +: this

  /**
   * Prepends elements to this path based on the specified string.
   *
   * @param string The regular string to derive elements from.
   * @return This path with elements prepended based on the specified string.
   */
  def +:(string: Regular): Path = extractElements(string) match {
    case Vector() => this
    case nonEmpty => copy(nonEmpty ++ elements)
  }

  /**
   * Prepends an element to this path.
   *
   * @param element The element to prepend.
   * @return This path with the specified element prepended.
   */
  def +:(element: Element): Path = copy(element +: elements)

  /**
   * Appends a child element with the specified name to this path.
   *
   * @param name The name of the child element to append.
   * @return This path with a named child appended.
   */
  def :+(name: Name): Path = this :+ Child(name)

  /**
   * Appends elements to this path based on the specified string.
   *
   * @param string The regular string to derive elements from.
   * @return This path with elements appended based on the specified string.
   */
  def :+(string: Regular): Path = extractElements(string) match {
    case Vector() => this
    case nonEmpty => copy(elements ++ nonEmpty)
  }

  /**
   * Appends an element to this path.
   *
   * @param element The element to append.
   * @return This path with the specified element appended.
   */
  def :+(element: Element): Path = copy(elements :+ element)

  /**
   * Appends a path to this path.
   *
   * @param that The path to append.
   * @return This path with the specified path appended.
   */
  def ++(that: Path): Path =
    if (elements.isEmpty) that
    else if (that.elements.isEmpty) this
    else copy(elements ++ that.elements)

  /* Convert to a string. */
  override def toString: String =
    if (elements.isEmpty) "" else elements.mkString("", "/", "/")

}

/**
 * Factory for paths.
 */
object Path {

  /** The empty path. */
  val empty: Path = Path(Vector.empty)

  /**
   * Constructs a path from the specified input.
   *
   * @tparam T The type of input.
   * @param input The input to create the path from.
   * @return The resulting path.
   */
  def apply[T: Builder](input: T*): Path =
    implicitly[Builder[T]].apply(input) match {
      case Seq() => empty
      case nonEmpty => Path(nonEmpty.toVector)
    }

  /**
   * Parses a path ending in '/' and also returns any meaningful trailing text.
   *
   * @param string The string to parse.
   * @return A path and any meaningful trailing text.
   */
  def parse(string: Regular): (Path, Option[String]) =
    if (string endsWith "/") {
      Path(string) -> None
    } else {
      val index = string.lastIndexOf('/') + 1
      val path = Path(string.substring(0, index))
      val suffix = string.substring(index)
      Element parse suffix collect {
        case append@(Parent | Current) => path :+ append
      } map (_ -> None) getOrElse path -> (if (suffix.isEmpty) None else Some(suffix))
    }

  /**
   * Parses a string into a sequence of elements.
   *
   * @param string The regular string to parse.
   * @return The parsed sequence of elements.
   */
  private def extractElements(string: Regular): Vector[Element] =
    string.split("\\/").toVector flatMap (Element.parse(_))

  /**
   * The base type for path elements.
   */
  sealed trait Element

  /**
   * Factory for path elements.
   */
  object Element {

    /**
     * Attempts to parse a path element.
     *
     * @param string The regular string to parse.
     * @return The parsed path element.
     */
    private[Path] def parse(string: Regular): Option[Element] = string.string match {
      case "." => Some(Current)
      case ".." => Some(Parent)
      case name => Name(name) map Child
    }

  }

  /**
   * The current path element.
   */
  case object Current extends Element {

    /* Convert to a string. */
    override def toString: String = "."

  }

  /**
   * The parent path element.
   */
  case object Parent extends Element {

    /* Convert to a string. */
    override def toString: String = ".."

  }

  /**
   * A named child path element.
   *
   * @param name The name of this child element.
   */
  case class Child(name: Name) extends Element {

    /* Convert to a string. */
    override def toString: String = name.toString

  }

  /**
   * Represents a path string that is in regular form (consistent path separators).
   *
   * @param string A regular path string.
   */
  final class Regular private(val string: String) extends AnyVal

  /**
   * Factory for regular path strings.
   */
  object Regular {

    /** All path strings can be made regular. */
    implicit def stringToRegular(string: String): Regular = apply(string)

    /** All regular strings are path strings. */
    implicit def regularToString(regular: Regular): String = regular.string

    /**
     * Ensures a path string is regular.
     *
     * @param string The path string that should be regular.
     * @return The regular path string.
     */
    def apply(string: String): Regular =
      new Regular(string.replaceAll("""[\\\/]+""", "/"))

  }

  /**
   * Type class for building element sequences.
   *
   * @tparam T The type to build elements from.
   */
  trait Builder[-T] {

    /**
     * Converts input into elements.
     *
     * @param input The input to convert.
     * @return The converted elements.
     */
    def apply(input: Seq[T]): Seq[Element]

  }

  /**
   * Definitions of the supported element builders.
   */
  object Builder {

    /** Builds elements from names. */
    implicit val Names: Builder[Name] = _ map Child

    /** Builds elements from regular path strings. */
    implicit val Regulars: Builder[Regular] = _ flatMap extractElements

    /** Builds elements from path strings. */
    implicit val Strings: Builder[String] = _ map (Regular(_)) flatMap extractElements

    /** Builds elements from path strings. */
    implicit val Elements: Builder[Element] = identity(_)

  }

}
