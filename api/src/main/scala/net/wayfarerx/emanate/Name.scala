/*
 * Name.scala
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
 * Represents a name with a normalized version of its display value.
 *
 * @param normal  The normalized name.
 * @param display The display name.
 */
final class Name private(val normal: String, val display: String) extends Product2[String, String] {

  /* Return the normal form. */
  override def _1: String =
    normal

  /* Return the display form. */
  override def _2: String =
    display

  /* Ignore the display name in equality checks. */
  override def canEqual(that: Any): Boolean =
    Option(that) collect { case Name(n, _) => n == normal } getOrElse false

  /* Ignore the display name in equality checks. */
  override def equals(obj: Any): Boolean =
    canEqual(obj)

  /* Ignore the display name in equality checks. */
  override def hashCode(): Int =
    Name.hashCode ^ normal.hashCode

  /* Return the normalized name. */
  override def toString: String =
    normal

}

/**
 * Factory for names.
 */
object Name extends (String => Option[Name]) {

  /**
   * Attempts to create a name.
   *
   * @param display The display name to normalize.
   * @return A new name if a non-empty name string is found.
   */
  override def apply(display: String): Option[Name] = {
    val normal = display.map {
      case c if c.isLetterOrDigit => c.toLower.toString
      case '\'' | '"' | '`' => ""
      case _ => " "
    }.mkString.trim.replaceAll("""\s+""", "-")
    if (normal.isEmpty) None else Some(new Name(normal, display))
  }

  /**
   * Extracts the contents of a name.
   *
   * @param name The name to extract.
   * @return The contents extracted from the specified name.
   */
  def unapply(name: Name): Option[(String, String)] =
    Some(name.normal, name.display)

}
