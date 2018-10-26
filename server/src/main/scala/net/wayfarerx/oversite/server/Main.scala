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
package server

import concurrent.ExecutionContext.Implicits.global

import cats.effect.IO

import fs2.{Stream, StreamApp}

/**
 * Main entry point for the oversite server.
 */
object Main extends StreamApp[IO] {

  /** The regex that describes a port. */
  private val PortPattern =
    s"""[0-9]+""".r

  /* Construct a stream that can only complete when the JVM is terminated. */
  def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, StreamApp.ExitCode] = args match {
    case className :: PortPattern(port) :: host :: Nil =>
      Stream.eval(IO(port.toInt)).flatMap(Server(className, _, host))
    case className :: host :: PortPattern(port) :: Nil =>
      Stream.eval(IO(port.toInt)).flatMap(Server(className, _, host))
    case className :: PortPattern(port) :: Nil =>
      Stream.eval(IO(port.toInt)).flatMap(Server(className, _))
    case className :: host :: Nil =>
      Server(className, host = host)
    case className :: Nil =>
      Server(className)
    case _ =>
      Stream {
        println("Usage: oversite <site-class> [host] [port]")
        StreamApp.ExitCode.Error
      }
  }

}
