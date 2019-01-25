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
package generator

import java.nio.file.Paths

import cats.effect.{ExitCode, IO, IOApp}
import org.slf4j.LoggerFactory
import model.Node

/**
 * The oversite server application.
 */
object Main extends IOApp {

  /** The name of this application. */
  private val name = "oversite-generator"

  /** The root application logger. */
  private val logger = LoggerFactory.getLogger(getClass)

  /** The command-line argument parser. */
  private val arguments = new scopt.OptionParser[Configuration](name) {
    head(name, "v0")
    arg[String]("<site>") text "the name of the class that describes the site" action { (s, c) =>
      c.copy(site = s)
    }
    arg[String]("destination").optional text "the directory to generate the site in" action { (d, c) =>
      c.copy(destination = d)
    }
  }

  /* Parse the command-line arguments and run the server. */
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      input <- IO(arguments.parse(args, Configuration()))
      config <- input map IO.pure getOrElse IO.raiseError(new IllegalArgumentException(args mkString " "))
      root <- Node.Root[AnyRef](config.site)
      destination <-
        if (config.destination.isEmpty) IO(Generator.DefaultDestination) else IO(Paths get config.destination)
      result <- Generator(root, destination)
    } yield result
  }.redeemWith(
    t => IO(logger.error("Oversite quit unexpectedly.", t)).redeem(_ => ExitCode.Error, _ => ExitCode.Error),
    IO.pure
  )

  /**
   * The configuration derived from the command-line arguments.
   *
   * @param site        The name of the class that implements `Site`.
   * @param destination The directory to generate the site in.
   */
  case class Configuration(
    site: String = "",
    destination: String = ""
  )

}
