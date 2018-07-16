/*
 * Scope.scala
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

import reflect.ClassTag

/**
 * A typed scope in a site.
 *
 * @tparam T The type of entities contained in this scope.
 * @param children The scope selectors for children of this scope.
 */
case class Scope[T <: AnyRef : ClassTag : Decoder : Publisher](
  children: Vector[(Scope.Select, Scope[_ <: AnyRef])]
) {

  /** The type of entities contained in this scope. */
  def classTag: ClassTag[T] = implicitly[ClassTag[T]]

  /** The decoder for the entities contained in this scope. */
  def decoder: Decoder[T] = implicitly[Decoder[T]]

  /** The publisher for the entities contained in this scope. */
  def publisher: Publisher[T] = implicitly[Publisher[T]]

  /**
   * Returns the specified child of this scope.
   *
   * @param name The name that identifies the child scope.
   * @return The scope for the specified child of this scope.
   */
  def apply(name: Name): Scope[_ <: AnyRef] = children collectFirst {
    case (Scope.Select.Matching(_name), child) if _name == name => child
    case (Scope.Select.All, child) => child
  } getOrElse (if (children.isEmpty) this else copy(Vector.empty))

}

/**
 * Factory for scopes.
 */
object Scope {

  /**
   * Creates a scope with all children using the specified child scope.
   *
   * @tparam T The type of entities contained in the scope.
   * @param children The scope to use for all children.
   * @return A new scope.
   */
  def apply[T <: AnyRef : ClassTag : Decoder : Publisher](children: Scope[_ <: AnyRef]): Scope[T] =
    Scope(Select.All -> children)

  /**
   * Creates a scope with the specified child scopes
   *
   * @tparam T The type of entities contained in the scope.
   * @param children The scopes to selectively use for children.
   * @return A new scope.
   */
  def apply[T <: AnyRef : ClassTag : Decoder : Publisher](children: (Select, Scope[_ <: AnyRef])*): Scope[T] =
    Scope[T](children.toVector)

  /**
   * A link to an external resource.
   *
   * @param href        The location of the resource.
   * @param integrity   The optional integrity hash.
   * @param crossorigin The optional cross-origin setting.
   */
  case class Resource(
    href: String,
    integrity: Option[String] = None,
    crossorigin: Option[String] = None
  )

  /**
   * Base type for scope selectors.
   */
  sealed trait Select

  /**
   * The supported selector types.
   */
  object Select {

    /**
     * Converts a string into a selector.
     *
     * @param str The string to convert.
     * @return A new scope selector.
     */
    def apply(str: String): Select =
      Name(str) map Matching getOrElse All

    /**
     * Selects all children.
     */
    case object All extends Select

    /**
     * Selects any matching children.
     *
     * @param name The name to match.
     */
    case class Matching(name: Name) extends Select

  }

}
