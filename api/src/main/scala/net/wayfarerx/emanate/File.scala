/*
 * File.scala
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
 * Describes a file contained in a location.
 *
 * @param location The location the file is contained in.
 * @param suffix The suffix that identifies the file in the location.
 */
final class File private(val location: Location, val suffix: String) extends Product2[Location, String] {

  /* Return the location. */
  override def _1: Location =
    location

  /* Return the suffix. */
  override def _2: String =
    suffix

  /* Check both the location and suffix. */
  override def canEqual(that: Any): Boolean =
    Option(that) collect { case File(l, s) => l == location && s == suffix } getOrElse false

  /* Check both the location and suffix. */
  override def equals(obj: Any): Boolean =
    canEqual(obj)

  /* Check both the location and suffix. */
  override def hashCode(): Int =
    File.hashCode ^ location.hashCode ^ suffix.hashCode

  /* Return the normalized name. */
  override def toString: String =
    s"$location/$suffix"

}

/**
 * Factory for files.
 */
object File extends ((Location, String) => Option[File]) {

  /**
   * Attempts to create a new file.
   *
   * @param location The location that contains the file.
   * @param suffix   The suffix that identifies the file.
   * @return The new file if the suffix is valid.
   */
  override def apply(location: Location, suffix: String): Option[File] =
    suffix.split("""[\\\/]+""") filterNot (_.isEmpty) match {
      case Array() => None
      case nonEmpty => Some(new File(location, nonEmpty mkString "/"))
    }

  /**
   * Extracts the contents of a file.
   *
   * @param file The file to extract.
   * @return The contents extracted from the specified file.
   */
  def unapply(file: File): Option[(Location, String)] =
    Some(file.location, file.suffix)

}
