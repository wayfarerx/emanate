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

import reflect.ClassTag

import cats.effect.IO

/**
 * Description of the context that a task operates in.
 */
trait Context {

  /** Returns the current location in the site. */
  def location: Location

  /** Attempts to return the stylesheets that are active in this context. */
  def stylesheets: IO[Vector[Styles]]

  /**
   * Attempts to return the fully resolved form of the specified author.
   *
   * @param author The author to resolve.
   * @return The result of attempting to resolve the specified author.
   */
  def resolve(author: Author): IO[Option[Author]]

  /**
   * Attempts to return the fully resolved form of the specified asset.
   *
   * @tparam T The type of asset to resolve.
   * @param asset The asset to resolve.
   * @return The result of attempting to resolve the specified asset.
   */
  def resolve[T <: Asset.Type](asset: Asset[T]): IO[Option[Asset.Resolved[T]]]

  /**
   * Attempts to return the fully resolved form of the specified entity.
   *
   * @tparam T The type of entity to resolve.
   * @param entity The entity to resolve.
   * @return The result of attempting to resolve the specified entity.
   */
  def resolve[T <: AnyRef : ClassTag](entity: Entity[T]): IO[Option[Entity.Resolved[T]]]

  /**
   * Attempts to load the data associated with the specified entity.
   *
   * @tparam T The type of entity to load.
   * @param entity The entity to load.
   * @return The result of attempting to load the data for the specified entity.
   */
  def load[T <: AnyRef : ClassTag](entity: Entity[T]): IO[Option[T]]

}
