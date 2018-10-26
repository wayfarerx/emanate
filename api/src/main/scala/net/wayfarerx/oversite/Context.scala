/*
 * Context.scala
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

import cats.effect.IO

/**
 * Description of the context that a task operates in.
 */
trait Context {

  /** Returns the site that contains this location. */
  def site: Site[_ <: AnyRef]

  /** Returns the current location in the site. */
  def location: Location

  /** Attempts to return all the stylesheets that are active in this context. */
  def stylesheets: Vector[Scope.Styles.Reference]

  /**
   * Attempts to return the fully resolved form of the specified author.
   *
   * @param author The author to resolve.
   * @return The result of attempting to resolve the specified author.
   */
  def resolve(author: Author): IO[Author]

  /**
   * Attempts to resolve the specified pointer.
   *
   * @tparam T The type of the pointer to resolve.
   * @param pointer The pointer to resolve.
   * @return The result of attempting to resolve the specified pointer.
   */
  def resolve[T <: Pointer.Type](pointer: Pointer[T]): IO[Pointer.Resolved[T]]

  /**
   * Attempts to resolve the specified pointer.
   *
   * @tparam T The type of the pointer to resolve.
   * @tparam S The type of suffix of the pointer to resolve.
   * @param pointer The pointer to resolve.
   * @return The result of attempting to resolve the specified pointer.
   */
  def resolve[T <: Pointer.Type.Aux[S], S](pointer: Pointer.Internal[T]): IO[Pointer.Target[T, S]]

  /**
   * Attempts to resolve the specified pointer.
   *
   * @tparam T The type of the pointer to resolve.
   * @param pointer The pointer to resolve.
   * @return The result of attempting to resolve the specified pointer.
   */
  def resolve[T <: Pointer.Asset](pointer: Pointer.External[T]): IO[Pointer.External[T]]

  /**
   * Attempts to load the data associated with the specified entity.
   *
   * @tparam T The type of entity to load.
   * @param entity The internal pointer to the entity to load.
   * @return The result of attempting to load the data for the specified entity.
   */
  def load[T <: AnyRef](entity: Pointer.Internal[Pointer.Entity[T]]): IO[T]

  /**
   * Attempts to load the alt-text for a specific image.
   *
   * @param image The resolved pointer to the image to load the alt-text for.
   * @return The result of attempting to load the alt-text for a specific image.
   */
  def alt(image: Pointer.Internal[Pointer.Image]): IO[Option[String]]

}
