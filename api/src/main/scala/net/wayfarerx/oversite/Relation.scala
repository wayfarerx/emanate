/*
 * Relation.scala
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
 * Base type for all relations from an entity to a collection of entity pointers.
 */
trait Relation[-T <: AnyRef] extends (T => Set[Pointer[Pointer.Entity[_ <: AnyRef]]])

/**
 * Factory for relations.
 */
object Relation {

  /**
   * Creates a relation defined by the specified function.
   *
   * @tparam T The type of entity the relation is from.
   * @param f The function that defines the relation.
   * @return A relation defined by the specified function.
   */
  def apply[T <: AnyRef](f: T => Set[Pointer[Pointer.Entity[_ <: AnyRef]]]): Relation[T] = f(_)

}
