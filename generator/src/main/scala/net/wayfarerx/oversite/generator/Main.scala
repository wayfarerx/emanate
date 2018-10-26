/*
 * Main.scala
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
package generator

import java.nio.file.Paths

import cats.effect.{ExitCode, IO, IOApp}

/**
 * Main entry point for the oversite generator.
 */
object Main extends IOApp {

  /* Generate the site and exit. */
  override def run(args: List[String]): IO[ExitCode] = args match {
    case className :: target :: Nil =>
      IO(Paths.get(target)) flatMap (Generator(className, _)) map (_ => ExitCode.Success)
    case className :: Nil =>
      IO(Paths.get("./target/oversite")) flatMap (Generator(className, _)) map (_ => ExitCode.Success)
    case _ =>
      IO(println("Usage: oversite <site-class> [target]")) map (_ => ExitCode.Error)
  }

}
