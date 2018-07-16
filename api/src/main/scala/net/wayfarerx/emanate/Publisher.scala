/*
 * Publisher.scala
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

import cats.effect.IO

/**
 * Defines how an entity is published to a site.
 *
 * @tparam T The type of entity being published.
 */
trait Publisher[-T <: AnyRef] {

  /**
   * Attempts to publish an entity to a site.
   *
   * @param entity The entity to publish.
   * @param ctx The context to publish in.
   * @return The result of attempting to publish an entity to a site.
   */
  def publish(entity: T)(implicit ctx: Context): IO[String]

}
