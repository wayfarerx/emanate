/*
 * TestServer.scala
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

import fs2.StreamApp

import scalatags.Text.all._

object TestServer extends StreamApp[IO] {

  override def stream(args: List[String], requestShutdown: IO[Unit]): fs2.Stream[IO, StreamApp.ExitCode] =
    Server(Website(name"test", name"wayfarerx", "http://localhost:4000", Scope[Home]()): Site)

  case class Website(name: Name, owner: Name, baseUrl: String, scopes: Scope[_ <: AnyRef]) extends Site

  case class Home(metadata: Markup.Metadata, image: Markup.Image)

  object Home {

    implicit val decoder: Decoder[Home] = new Decoder[Home] {
      override def decode(doc: Markup.Document)(implicit ctx: Context): IO[Home] = {
        println(doc)
        doc match {
          case Markup.Document(metadata, Vector(Markup.Paragraph(Vector(image: Markup.Image))), Vector()) =>
            IO.pure(Home(metadata, image))
          case _ =>
            IO.raiseError(new IllegalArgumentException(s"Unacceptable home document."))
        }
      }
    }

    implicit val publisher: Publisher[Home] = new Publisher[Home] with utils.Page[Home] {

      import utils.Page._

      override def publish(entity: Home)(implicit ctx: Context): IO[String] = apply(entity)

      override protected def pageMetadata(entity: Home): Markup.Metadata = entity.metadata

      override protected def pageContent(entity: Home)(implicit ctx: Context): IO[Frag] =
        entity.image.renderImg()

    }

  }

}
