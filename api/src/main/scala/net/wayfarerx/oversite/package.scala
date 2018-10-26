/*
 * package.scala
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

package net.wayfarerx

/**
 * Global definitions for the oversite framework.
 */
package object oversite {

  /**
   * Adds a compile-time name generator. Care must be taken so that names declared in this fashion are never empty after
   * interpolation and normalization or it will result in runtime exceptions.
   *
   * @param ctx the string context to extend.
   */
  final implicit class NameInterpolator(val ctx: StringContext) extends AnyVal {

    /** Build a name from the context and arguments. */
    def name(args: Any*): Name = {
      val name = ctx.standardInterpolator(identity, args)
      Name(name) getOrElse (throw new IllegalArgumentException(s"Invalid name: $name"))
    }

  }

}
