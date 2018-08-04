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
  lazy val parent: Option[Location] =
    Location(path.parent.normalized)

  /**
   * Extends this location.
   *
   * @param name The name to extend this location with.
   * @return The extended location.
   */
  def :+ (name: Name): Location =
    new Location(path :+ name)

  /**
   * Returns the longest prefix shared between this and that location.
   *
   * @param that The location to compare against.
   * @return The longest prefix shared between this and that location.
   */
  def commonPrefixWith(that: Location): Path = {
    val max = Math.min(path.elements.length, that.path.elements.length)
    Path(path.elements.iterator.take(max).zip(that.path.elements.iterator.take(max)).takeWhile {
      case (ours, theirs) => ours == theirs
    }.map(_._1).toVector)
  }

  /**
   * Calculates the distance to another location.
   *
   * @param that The location to measure the distance to.
   * @return The distance to the specified location.
   */
  def distanceTo(that: Location): Int = {
    val common = commonPrefixWith(that)
    (path.elements.length - common.elements.length) - (that.path.elements.length - common.elements.length)
  }

  /* Return the path. */
  override def _1: Path =
    path

  /* Check for matching paths. */
  override def canEqual(that: Any): Boolean =
    Option(that) collect { case Location(p) => p == path } getOrElse false

  /* Check for matching paths. */
  override def equals(obj: Any): Boolean =
    canEqual(obj)

  /* Check for matching paths. */
  override def hashCode(): Int =
    Location.hashCode ^ path.hashCode

  /* Return the absolute path. */
  override def toString: String =
    s"/$path/"

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
   * @return A new name if a resolvable path was provided.
   */
  override def apply(path: Path): Option[Location] = {
    val normalized = path.normalized
    if (normalized.isResolved) Some(new Location(normalized)) else None
  }

  /**
   * Extracts the contents of a location.
   *
   * @param location The location to extract.
   * @return The contents extracted from the specified location.
   */
  def unapply(location: Location): Option[Path] =
    Some(location.path)

}
