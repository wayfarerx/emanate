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

import cats.effect.IO

/**
 * A typed scope in a site.
 *
 * @tparam T The type of entities contained in this scope.
 * @param stylesheets The stylesheet collection for this scope.
 * @param children    The scope selectors for children of this scope.
 * @param indexed     True if this scope is indexed.
 */
case class Scope[T <: AnyRef : ClassTag : Decoder : Publisher](
  stylesheets: Vector[Scope.Styles],
  children: Vector[(Scope.Select, Scope[_ <: AnyRef])],
  indexed: Boolean
) {

  /** The type of entities contained in this scope. */
  def classTag: ClassTag[T] = implicitly[ClassTag[T]]

  /** The decoder for the entities contained in this scope. */
  def decoder: Decoder[T] = implicitly[Decoder[T]]

  /** The publisher for the entities contained in this scope. */
  def publisher: Publisher[T] = implicitly[Publisher[T]]

  /** Returns this scope as an unspecified extension of itself. */
  def extended: Scope[T] = if (stylesheets.isEmpty) this else copy(Vector.empty)

  /**
   * Returns the specified child of this scope.
   *
   * @param name The name that identifies the child scope.
   * @return The scope for the specified child of this scope.
   */
  def apply(name: Name): Scope[_ <: AnyRef] = search(name) getOrElse extended

  /**
   * Returns the specified child of this scope if one exists.
   *
   * @param name The name that identifies the child scope.
   * @return The specified child of this scope if one exists.
   */
  def search(name: Name): Option[Scope[_ <: AnyRef]] = children collectFirst {
    case (Scope.Select.Matching(n), child) if n == name => child
    case (Scope.Select.All, child) => child
  }

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
  def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
    children: Scope[_ <: AnyRef]
  ): Scope[T] =
    Scope(Select.All -> children)

  /**
   * Creates a scope with the specified child scopes
   *
   * @tparam T The type of entities contained in the scope.
   * @param children The scopes to selectively use for children.
   * @return A new scope.
   */
  def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
    children: (Select, Scope[_ <: AnyRef])*
  ): Scope[T] =
    Scope(Vector.empty, children.toVector, indexed = true)

  /**
   * Creates a scope with stylesheets and all children using the specified child scope.
   *
   * @tparam T The type of entities contained in the scope.
   * @param stylesheets The stylesheet collection for the scope.
   * @param children    The scope to use for all children.
   * @return A new scope.
   */
  def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
    stylesheets: Vector[Styles],
    children: Scope[_ <: AnyRef]
  ): Scope[T] =
    Scope(stylesheets, Select.All -> children)

  /**
   * Creates a scope with stylesheets and the specified child scopes.
   *
   * @tparam T The type of entities contained in the scope.
   * @param stylesheets The stylesheet collection for the scope.
   * @param children    The scopes to selectively use for children.
   * @return A new scope.
   */
  def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
    stylesheets: Vector[Styles],
    children: (Select, Scope[_ <: AnyRef])*
  ): Scope[T] =
    Scope(stylesheets, children.toVector, indexed = true)

  /**
   * Base type for scope selectors.
   */
  sealed trait Select

  /**
   * The supported selector types.
   */
  object Select {

    /** Returns the all selector. */
    def apply(): Select = All

    /**
     * Converts a name into a selector.
     *
     * @param name The name to convert.
     * @return A new scope selector.
     */
    def apply(name: Name): Select = Matching(name)

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

  /**
   * Base class for stylesheet declarations.
   */
  sealed trait Styles

  /**
   * Definitions of the supported stylesheet references.
   */
  object Styles {

    /**
     * A stylesheet that can be generated on demand.
     *
     * @param name     The name of this stylesheet in its scope.
     * @param generate The function that generates the stylesheet.
     */
    case class Generated(name: Name, generate: Context => IO[String]) extends Styles

    /**
     * Base class for references to stylesheets.
     */
    sealed trait Reference extends Styles

    /**
     * A stylesheet that is stored within the site.
     *
     * @param pointer The pointer that identifies the internal stylesheet.
     */
    case class Internal(pointer: Pointer.Internal[Pointer.Stylesheet]) extends Reference

    /**
     * A stylesheet that is stored outside the site.
     *
     * @param pointer     The pointer that identifies the external stylesheet.
     * @param integrity   The assertion about the stylesheet's integrity.
     * @param crossorigin The crossorigin configuration.
     */
    case class External(
      pointer: Pointer.External[Pointer.Stylesheet],
      integrity: Option[String] = None,
      crossorigin: Option[String] = None
    ) extends Reference

  }

}
