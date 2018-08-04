/*
 * EnvironmentSupport.scala
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
package model

import concurrent.ExecutionContext

/**
 * Support for tests that require environments.
 */
trait EnvironmentSupport {

  /** The immediate mode executor. */
  private val executor = new ExecutionContext {

    override def execute(runnable: Runnable): Unit =
      runnable.run()

    override def reportFailure(cause: Throwable): Unit =
      cause.printStackTrace()

  }

  /** The environment to use when testing. */
  val environment = Environment(getClass.getClassLoader, executor, executor)

}
