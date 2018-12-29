/*
 * Site.scala
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
 * Describes an entire site.
 */
trait Site[T <: AnyRef] extends (Location => Scope[_ <: AnyRef]) {

  /** The name of the site. */
  def name: Name

  /** The author of the site. */
  def owner: Author

  /** The base URL to append absolute paths to. */
  def baseUrl: String

  /** The entry point for the scopes that describe this site. */
  def scopes: Scope[T]

  /**
   * Return the scope at the specified location.
   *
   * @param location The location of the scope to return.
   * @return The scope at the specified location.
   */
  final override def apply(location: Location = Location.empty): Scope[_ <: AnyRef] = {

    @annotation.tailrec
    def search(scope: Scope[_ <: AnyRef], remaining: Vector[Name]): Scope[_ <: AnyRef] = remaining match {
      case head +: tail => search(scope.find(head) getOrElse scope.extended, tail)
      case _ => scope
    }

    search(scopes, location.names)
  }

}
