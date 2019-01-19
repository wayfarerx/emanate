/*
 * Main.scala
 *
 * Copyright 2019 wayfarerx <x@wayfarerx.net> (@thewayfarerx)
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

import cats.effect.{ExitCode, IO, IOApp}

import org.slf4j.LoggerFactory

import model.Node

/**
 * The oversite server application.
 */
object Main extends IOApp {

  /** The name of this application. */
  private val name = "oversite-server"

  /** The root application logger. */
  private val logger = LoggerFactory.getLogger(getClass)

  /** The command-line argument parser. */
  private val arguments = new scopt.OptionParser[Configuration](name) {
    head(name, "v0")
    arg[String]("<site>") action ((s, c) => c.copy(site = s)) text "the name of the class that implements Site"
    arg[Int]("port").optional action ((p, c) => c.copy(port = p)) text "the port to serve on"
    arg[String]("host").optional action ((h, c) => c.copy(host = h)) text "the hostname to serve on"
  }

  /* Parse the command-line arguments and run the server. */
  override def run(args: List[String]): IO[ExitCode] =
    arguments.parse(args, Configuration()) map { config =>
      Node.Root[AnyRef](config.site).flatMap(r => Server(r, config.host, config.port)).redeemWith(
        t => IO(logger.error("Oversite quit unexpectedly.", t)).redeem(_ => ExitCode.Error, _ => ExitCode.Error),
        IO.pure
      )
    } getOrElse IO.pure(ExitCode.Error)

  /**
   * The configuration derived from the command-line arguments.
   *
   * @param site The name of the class that implements `Site`.
   * @param host The hostname to serve on.
   * @param port The port to serve on.
   */
  case class Configuration(
    site: String = "",
    port: Int = Server.DefaultPort,
    host: String = Server.DefaultHost
  )

}
