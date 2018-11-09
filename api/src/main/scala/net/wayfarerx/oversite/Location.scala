/*
 * Location.scala
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
 * Base class for resolved locations.
 */
final class Location private(val path: Path) extends Product1[Path] {

  /** The parent location. */
  def parent: Option[Location] = Location(path.parent)

  /** The names that make up this location. */
  def names: Vector[Name] = path.elements collect { case Path.Child(name) => name }

  /* Return the path. */
  override def _1: Path = path

  /**
   * Extends this location with a name.
   *
   * @param name The name to extend this location with.
   * @return The extended location.
   */
  def :+(name: Name): Location = new Location(path :+ name)

  /**
   * Attempts to extend this location with a path.
   *
   * @param path The path to extend this location with.
   * @return The extended location.
   */
  def :++(path: Path): Option[Location] =
    Location(this.path ++ path)

  /**
   * Returns the longest prefix shared between this and that location.
   *
   * @param that The location to compare against.
   * @return The longest prefix shared between this and that location.
   */
  def commonPrefixWith(that: Location): Location = {
    val self = names
    val other = that.names
    val max = Math.min(self.length, other.length)
    new Location(Path(self.take(max) zip other.take(max) takeWhile {
      case (ours, theirs) => ours == theirs
    } map (_._1): _*))
  }

  /**
   * Returns the path that moves from this location to that location.
   *
   * @param that The location to move to.
   * @return The path that moves from this location to that location.
   */
  def pathTo(that: Location): Path = {
    val common = commonPrefixWith(that)
    Path(
      Vector.fill(path.elements.length - common.path.elements.length)(Path.Parent) ++
        that.path.elements.drop(common.path.elements.length)
    )
  }

  /**
   * Calculates the distance to another location.
   *
   * @param that The location to measure the distance to.
   * @return The distance to the specified location.
   */
  def distanceTo(that: Location): Int = {
    val common = commonPrefixWith(that)
    (path.elements.length - common.path.elements.length) + (that.path.elements.length - common.path.elements.length)
  }

  /* Check for matching paths. */
  override def canEqual(that: Any): Boolean = that match {
    case Location(_) => true
    case _ => false
  }

  /* Check for matching paths. */
  override def equals(that: Any): Boolean = that match {
    case l@Location(p) if l canEqual this => p == path
    case _ => false
  }

  /* Check for matching paths. */
  override def hashCode(): Int =
    Location.hashCode ^ path.hashCode

  /* Return the absolute path. */
  override def toString: String = s"/$path"

}

/**
 * Facgtory for locations.
 */
object Location extends (Path => Option[Location]) {

  /** The empty location. */
  val empty = new Location(Path.empty)

  /**
   * Attempts to create a location.
   *
   * @param path The path of the location to create.
   * @return A new location if a resolvable path was provided.
   */
  override def apply(path: Path): Option[Location] =
    Some(path.normalized) filter (_.isResolved) map (new Location(_))

  /**
   * Creates a location from a resolved path.
   *
   * @param path The resolved path of the location to create.
   * @return A new location for the resolved path.
   */
  def resolved(path: Path): Location =
    new Location(path.resolved)

  /**
   * Extracts the contents of a location.
   *
   * @param location The location to extract.
   * @return The contents extracted from the specified location.
   */
  def unapply(location: Location): Option[Path] =
    Some(location.path)

}
