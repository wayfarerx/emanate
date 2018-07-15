/*
 * Entity.scala
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

import scala.reflect.ClassTag

/**
 * Base class for references to entities.
 *
 * @tparam T The expected type of the desired entity.
 */
sealed trait Entity[T <: AnyRef] {

  /** The tag of the type that the entity must be assignable to. */
  def assignableTo: ClassTag[T]

  /**
   * Creates a copy of this entity reference with a narrower type.
   *
   * @tparam U The narrowed type of this entity reference.
   * @return A copy of this entity reference with a narrower type.
   */
  def narrow[U <: T : ClassTag]: Entity[U]

}

/**
 * Definitions of the supported entity references.
 */
object Entity {

  /**
   * Attempts to create a new entity from the specified string.
   *
   * @tparam T The expected type of the desired entity.
   * @param string The string to parse.
   * @return A new entity if one could be created.
   */
  def apply[T <: AnyRef : ClassTag](string: String): Option[Entity[T]] = string match {
    case absolute if absolute startsWith "/" => Location(Path(absolute)) map Entity.Absolute[T]
    case relative if relative contains "/" => Some(Entity.Relative[T](Path(relative)))
    case named => Name(named) map Entity.Named[T]
  }

  /**
   * A reference to an entity by name.
   *
   * @tparam T The expected type of the desired entity.
   * @param name The name of the desired entity.
   */
  case class Named[T <: AnyRef : ClassTag](name: Name) extends Entity[T] {

    /* The tag of the expected type of the desired entity. */
    override def assignableTo: ClassTag[T] = implicitly[ClassTag[T]]

    /* Return the narrowed entity reference. */
    override def narrow[U <: T : ClassTag]: Named[U] = Named[U](name)

  }

  /**
   * Base class for resolved references to entities.
   *
   * @tparam T The expected type of the desired entity.
   */
  sealed trait Resolved[T <: AnyRef] extends Entity[T]

  /**
   * A reference to an entity by relative path.
   *
   * @tparam T The expected type of the desired entity.
   * @param path The path to the desired entity.
   */
  case class Relative[T <: AnyRef : ClassTag](path: Path) extends Resolved[T] {

    /* The tag of the expected type of the desired entity. */
    override def assignableTo: ClassTag[T] = implicitly[ClassTag[T]]

    /* Return the narrowed entity reference. */
    override def narrow[U <: T : ClassTag]: Relative[U] = Relative[U](path)

  }

  /**
   * A reference to an entity by absolute path.
   *
   * @tparam T The expected type of the desired entity.
   * @param location The location of the desired entity.
   */
  case class Absolute[T <: AnyRef : ClassTag](location: Location) extends Resolved[T] {

    /* The tag of the expected type of the desired entity. */
    override def assignableTo: ClassTag[T] = implicitly[ClassTag[T]]

    /* Return the narrowed entity reference. */
    override def narrow[U <: T : ClassTag]: Absolute[U] = Absolute[U](location)

  }

}