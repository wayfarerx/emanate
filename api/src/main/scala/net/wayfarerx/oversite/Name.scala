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

package net.wayfarerx.oversite

/**
 * Represents a name with a normalized version of its display value.
 *
 * @param normal  The normalized name.
 * @param display The display name.
 */
final class Name private(val normal: String, val display: String) extends Product2[String, String] {

  /* Return the normal form. */
  override def _1: String = normal

  /* Return the display form. */
  override def _2: String = display

  /* Ignore the display name in equality checks. */
  override def canEqual(that: Any): Boolean = that match {
    case Name(_, _) => true
    case _ => false
  }

  /* Ignore the display name in equality checks. */
  override def equals(that: Any): Boolean = that match {
    case n@Name(nn, _) if n canEqual this => nn == normal
    case _ => false
  }

  /* Ignore the display name in equality checks. */
  override def hashCode(): Int =
    Name.hashCode ^ normal.hashCode

  /* Return the normalized name. */
  override def toString: String = normal

}

/**
 * Factory for names.
 */
object Name extends (String => Option[Name]) {

  /**
   * Attempts to create a name.
   *
   * @param string The string create a name from.
   * @return A new name if a valid name was found.
   */
  override def apply(string: String): Option[Name] =
    string.map {
      case c if c.isLetterOrDigit => c.toLower.toString
      case '\'' | '"' | '`' => ""
      case _ => " "
    }.mkString.trim.replaceAll("""\s+""", "-") match {
      case e if e.isEmpty => None
      case n => Some(new Name(n, string))
    }

  /**
   * Extracts the contents of a name.
   *
   * @param name The name to extract.
   * @return The contents extracted from the specified name.
   */
  def unapply(name: Name): Option[(String, String)] = Some(name._1, name._2)

}
