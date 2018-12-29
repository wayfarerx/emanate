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
 * Base type for scopes in a site.
 *
 * @tparam T The type of entity contained in this scope.
 */
sealed trait Scope[T <: AnyRef] {

  /** The type of entities contained in this scope. */
  def classTag: ClassTag[T]

  /** The decoder for the entities contained in this scope. */
  def decoder: Decoder[T]

  /** The publisher for the entities contained in this scope. */
  def publisher: Publisher[T]

  /** True if the children of this scope are indexed. */
  def indexed: Boolean

  /** The asset generators for this scope. */
  def generators: Vector[Scope.Generator]

  /** The selectors for children of this scope. */
  def children: Vector[(Scope.Select, Scope[_ <: AnyRef])]

  /** Returns this scope as an unspecified extension of itself. */
  def extended: Scope[T]

  /**
   * Returns the specified child of this scope.
   *
   * @param name The name that identifies the child scope.
   * @return The scope for the specified child of this scope.
   */
  final def apply(name: Name): Scope[_ <: AnyRef] =
    find(name) getOrElse extended

  /**
   * Returns the specified child of this scope if one exists.
   *
   * @param name The name that identifies the child scope.
   * @return The specified child of this scope if one exists.
   */
  final def find(name: Name): Option[Scope[_ <: AnyRef]] = children collectFirst {
    case (Scope.Select.Matching(n), child) if n == name => child
    case (Scope.Select.All, child) => child
  }

}

/**
 * Factory for scopes.
 */
object Scope {

  /** The default indexed setting. */
  def DefaultIndexed: Boolean = true

  /** The default generators setting. */
  def DefaultGenerators: Vector[Generator] = Vector.empty

  /**
   * Creates an indexed scope with all children using the specified child scope.
   *
   * @tparam T The type of entities contained in the scope.
   * @param children The scope to use for all children.
   * @return A new scope.
   */
  def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
    children: Scope[_ <: AnyRef]
  ): Nested[T] = apply(DefaultIndexed, DefaultGenerators, children)

  /**
   * Creates an indexed scope with the specified child scopes.
   *
   * @tparam T The type of entities contained in the scope.
   * @param children The scopes to selectively use for children.
   * @return A new scope.
   */
  def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
    children: (Select, Scope[_ <: AnyRef])*
  ): Nested[T] = apply(DefaultIndexed, DefaultGenerators, children.toVector: _*)

  /**
   * Creates a possibly indexed scope with all children using the specified child scope.
   *
   * @tparam T The type of entities contained in the scope.
   * @param indexed  True if the scope's contents are indexed.
   * @param children The scope to use for all children.
   * @return A new scope.
   */
  def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
    indexed: Boolean,
    children: Scope[_ <: AnyRef]
  ): Nested[T] = apply(indexed, DefaultGenerators, children)

  /**
   * Creates a possibly indexed scope with the specified child scopes.
   *
   * @tparam T The type of entities contained in the scope.
   * @param indexed  True if the scope's contents are indexed.
   * @param children The scopes to selectively use for children.
   * @return A new scope.
   */
  def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
    indexed: Boolean,
    children: (Select, Scope[_ <: AnyRef])*
  ): Nested[T] = apply(indexed, DefaultGenerators, children.toVector: _*)

  /**
   * Creates an indexed scope with asset generators and all children using the specified child scope.
   *
   * @tparam T The type of entities contained in the scope.
   * @param generators The asset generators for the scope.
   * @param children   The scope to use for all children.
   * @return A new scope.
   */
  def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
    generators: Vector[Generator],
    children: Scope[_ <: AnyRef]
  ): Nested[T] = apply(DefaultIndexed, generators, children)

  /**
   * Creates an indexed scope with asset generators and the specified child scopes.
   *
   * @tparam T The type of entities contained in the scope.
   * @param generators The asset generators for the scope.
   * @param children   The scopes to selectively use for children.
   * @return A new scope.
   */
  def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
    generators: Vector[Generator],
    children: (Select, Scope[_ <: AnyRef])*
  ): Nested[T] = apply(DefaultIndexed, generators, children.toVector: _*)

  /**
   * Creates a scope with asset generators and all children using the specified child scope.
   *
   * @tparam T The type of entities contained in the scope.
   * @param indexed    True if the scope's contents are indexed.
   * @param generators The asset generators for the scope.
   * @param children   The scope to use for all children.
   * @return A new scope.
   */
  def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
    indexed: Boolean,
    generators: Vector[Generator],
    children: Scope[_ <: AnyRef]
  ): Nested[T] = apply(indexed, generators, Select.All -> children)

  /**
   * Creates a scope with asset generators and the specified child scopes.
   *
   * @tparam T The type of entities contained in the scope.
   * @param indexed    True if the scope's contents are indexed.
   * @param generators The asset generators for the scope.
   * @param children   The scopes to selectively use for children.
   * @return A new scope.
   */
  def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
    indexed: Boolean,
    generators: Vector[Generator],
    children: (Select, Scope[_ <: AnyRef])*
  ): Nested[T] = Nested(indexed, generators, children.toVector)

  /**
   * The base class for scope implementations.
   *
   * @tparam T The type of entity contained in this scope.
   */
  sealed abstract class Base[T <: AnyRef : ClassTag : Decoder : Publisher] extends Scope[T] {

    /* Return the class tag. */
    final override def classTag: ClassTag[T] = implicitly[ClassTag[T]]

    /* Return the decoder. */
    final override def decoder: Decoder[T] = implicitly[Decoder[T]]

    /* Return the publisher. */
    final override def publisher: Publisher[T] = implicitly[Publisher[T]]

  }

  /**
   * A scope nested within another scope.
   *
   * @tparam T The type of entity contained in this scope.
   * @param indexed    True if the scope's contents are indexed.
   * @param generators The asset generators for the scope.
   * @param children   The scopes to selectively use for children.
   */
  case class Nested[T <: AnyRef : ClassTag : Decoder : Publisher](
    indexed: Boolean,
    generators: Vector[Generator],
    children: Vector[(Select, Scope[_ <: AnyRef])]
  ) extends Base[T] {

    /* Extend this type by dropping the generators. */
    override def extended: Nested[T] =
      if (generators.isEmpty) this else copy(generators = Vector.empty)

  }

  /**
   * A scope nested within another scope.
   *
   * @tparam T The type of entity contained in this scope.
   * @param path       The path that this scope is found at.
   * @param indexed    True if the scope's contents are indexed.
   * @param generators The asset generators for the scope.
   * @param children   The scopes to selectively use for children.
   */
  case class Aliased[T <: AnyRef : ClassTag : Decoder : Publisher](
    path: Path,
    indexed: Boolean,
    generators: Vector[Generator],
    children: Vector[(Select, Scope[_ <: AnyRef])]
  ) extends Base[T] {

    /* Extend this type by converting to a nested scope. */
    override def extended: Nested[T] =
      Nested(indexed, Vector.empty, children)

  }

  /**
   * Factory for aliased scopes.
   */
  object Aliased {

    /**
     * Creates an indexed scope with all children using the specified child scope.
     *
     * @tparam T The type of entities contained in the scope.
     * @param path     The path that provides the resources for this scope.
     * @param children The scope to use for all children.
     * @return A new scope.
     */
    def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
      path: Path,
      children: Scope[_ <: AnyRef]
    ): Aliased[T] = apply(path, Select.All -> children)

    /**
     * Creates an indexed scope with the specified child scopes.
     *
     * @tparam T The type of entities contained in the scope.
     * @param path     The path that provides the resources for this scope.
     * @param children The scopes to selectively use for children.
     * @return A new scope.
     */
    def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
      path: Path,
      children: (Select, Scope[_ <: AnyRef])*
    ): Aliased[T] = Aliased(path, DefaultIndexed, DefaultGenerators, children.toVector)

    /**
     * Creates a possibly indexed scope with all children using the specified child scope.
     *
     * @tparam T The type of entities contained in the scope.
     * @param path     The path that provides the resources for this scope.
     * @param indexed  True if the scope's contents are indexed.
     * @param children The scope to use for all children.
     * @return A new scope.
     */
    def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
      path: Path,
      indexed: Boolean,
      children: Scope[_ <: AnyRef]
    ): Aliased[T] = apply(path, indexed, Select.All -> children)

    /**
     * Creates a possibly indexed scope with the specified child scopes.
     *
     * @tparam T The type of entities contained in the scope.
     * @param path     The path that provides the resources for this scope.
     * @param indexed  True if the scope's contents are indexed.
     * @param children The scopes to selectively use for children.
     * @return A new scope.
     */
    def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
      path: Path,
      indexed: Boolean,
      children: (Select, Scope[_ <: AnyRef])*
    ): Aliased[T] = Aliased(path, indexed, DefaultGenerators, children.toVector)

    /**
     * Creates an indexed scope with asset generators and all children using the specified child scope.
     *
     * @tparam T The type of entities contained in the scope.
     * @param path       The path that provides the resources for this scope.
     * @param generators The asset generators for the scope.
     * @param children   The scope to use for all children.
     * @return A new scope.
     */
    def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
      path: Path,
      generators: Vector[Generator],
      children: Scope[_ <: AnyRef]
    ): Aliased[T] = apply(path, generators, Select.All -> children)

    /**
     * Creates an indexed scope with asset generators and the specified child scopes.
     *
     * @tparam T The type of entities contained in the scope.
     * @param path       The path that provides the resources for this scope.
     * @param generators The asset generators for the scope.
     * @param children   The scopes to selectively use for children.
     * @return A new scope.
     */
    def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
      path: Path,
      generators: Vector[Generator],
      children: (Select, Scope[_ <: AnyRef])*
    ): Aliased[T] = Aliased(path, DefaultIndexed, generators, children.toVector)

    /**
     * Creates a scope with asset generators and all children using the specified child scope.
     *
     * @tparam T The type of entities contained in the scope.
     * @param path       The path that provides the resources for this scope.
     * @param indexed    True if the scope's contents are indexed.
     * @param generators The asset generators for the scope.
     * @param children   The scope to use for all children.
     * @return A new scope.
     */
    def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
      path: Path,
      indexed: Boolean,
      generators: Vector[Generator],
      children: Scope[_ <: AnyRef]
    ): Aliased[T] = apply(path, indexed, generators, Select.All -> children)

    /**
     * Creates a scope with asset generators and the specified child scopes.
     *
     * @tparam T The type of entities contained in the scope.
     * @param path       The path that provides the resources for this scope.
     * @param indexed    True if the scope's contents are indexed.
     * @param generators The asset generators for the scope.
     * @param children   The scopes to selectively use for children.
     * @return A new scope.
     */
    def apply[T <: AnyRef : ClassTag : Decoder : Publisher](
      path: Path,
      indexed: Boolean,
      generators: Vector[Generator],
      children: (Select, Scope[_ <: AnyRef])*
    ): Aliased[T] = Aliased(path, indexed, generators, children.toVector)

  }

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
   * An asset generator registered with a scope.
   *
   * @param name     The name of the generated asset.
   * @param tpe      The type of the generated asset.
   * @param generate The function that generates the asset data.
   */
  case class Generator(name: Name, tpe: Pointer.Asset#Variant, generate: Context => IO[Array[Byte]])

}
