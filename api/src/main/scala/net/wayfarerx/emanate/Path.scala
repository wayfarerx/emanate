/*
 * Path.scala
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
 * A path within the scope of a site.
 *
 * @param elements The elements that comprise this path.
 */
case class Path(elements: Vector[Path.Element]) {

  import Path._

  /** True if this path is empty. */
  def isEmpty: Boolean =
    elements.isEmpty

  /** True if this path is not empty. */
  def nonEmpty: Boolean =
    elements.nonEmpty

  /**
   * True if this path is normal (has no '.' elements).
   *
   * @return True if this path is normal.
   */
  def isNormal: Boolean =
    !elements.contains(Current) && !elements.dropWhile(_ == Parent).contains(Parent)

  /**
   * True if this path is resolved (has no '.' or '..' elements).
   *
   * @return True if this path is resolved.
   */
  def isResolved: Boolean =
    !elements.exists(e => e == Current || e == Parent)

  /** The parent of this path. */
  def parent: Path =
    this :+ Parent

  /**
   * Creates a path to the specified child.
   *
   * @param name The name of the child.
   * @return A path to the specified child.
   */
  def apply(name: Name): Path =
    this :+ name

  /**
   * Resolves a path, starting at this path, using the specified string.
   *
   * @param string The string to resolve against this path.
   * @return A path, starting at this path, using the specified string.
   */
  def apply(string: String): Path =
    this :+ string

  /**
   * Prepends elements to this path based on the specified string.
   *
   * @param string The string to derive elements from.
   * @return This path with elements prepended based on the specified string.
   */
  def +:(string: String): Path =
    stringToElements(string) match {
      case Vector() => this
      case nonEmpty => copy(nonEmpty ++ elements)
    }

  /**
   * Prepends a child element with the specified name to this path.
   *
   * @param name The name of the child element to prepend.
   * @return This path with a named child prepended.
   */
  def +:(name: Name): Path =
    Child(name) +: this

  /**
   * Prepends an element to this path.
   *
   * @param element The element to prepend.
   * @return This path with the specified element prepended.
   */
  def +:(element: Element): Path =
    copy(element +: elements)

  /**
   * Appends elements to this path based on the specified string.
   *
   * @param string The string to derive elements from.
   * @return This path with elements appended based on the specified string.
   */
  def :+(string: String): Path =
    stringToElements(string) match {
      case Vector() => this
      case nonEmpty => copy(elements ++ nonEmpty)
    }

  /**
   * Appends a child element with the specified name to this path.
   *
   * @param name The name of the child element to append.
   * @return This path with a named child appended.
   */
  def :+(name: Name): Path =
    this :+ Child(name)

  /**
   * Appends an element to this path.
   *
   * @param element The element to append.
   * @return This path with the specified element appended.
   */
  def :+(element: Element): Path =
    copy(elements :+ element)

  /**
   * Appends a path to this path.
   *
   * @param that The path to append.
   * @return This path with the specified path appended.
   */
  def ++(that: Path): Path =
    copy(elements ++ that.elements)

  /**
   * Returns the normalized version of this path.
   *
   * @return The normalized version of this path.
   */
  def normalized: Path =
    if (isNormal) this else Path((Vector[Element]() /: elements) { (result, element) =>
      element match {
        case Current => result
        case Parent if result.isEmpty || result.last == Parent => result :+ Parent
        case Parent => result.init
        case child@Child(_) => result :+ child
      }
    })

  /* Convert to a string. */
  override def toString: String =
    elements mkString "/"

}

/**
 * Factory for paths.
 */
object Path {

  /** The empty path. */
  val empty: Path = Path(Vector.empty)

  /**
   * Parses a string into a sequence of elements.
   *
   * @param string The string to parse.
   * @return The parsed sequence of elements.
   */
  private def stringToElements(string: String): Vector[Element] =
    string.split("""[\\\/]+""").toVector flatMap {
      case "." => Some(Current)
      case ".." => Some(Parent)
      case name => Name(name) map Child
    }

  /**
   * Parses a path from the specified string.
   *
   * @param string The string to parse the path from.
   * @return The parsed path.
   */
  def apply(string: String): Path =
    stringToElements(string) match {
      case Vector() => empty
      case nonEmpty => Path(nonEmpty)
    }

  /**
   * Creates a path from the specified names.
   *
   * @param names The names to create the path from.
   * @return The new path.
   */
  def apply(names: Name*): Path =
    if (names.isEmpty) empty
    else Path(names.toVector map Child)

  /**
   * The base type for path elements.
   */
  sealed trait Element

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

}
