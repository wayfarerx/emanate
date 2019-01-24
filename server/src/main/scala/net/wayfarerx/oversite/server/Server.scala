/*
 * Server.scala
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

import java.util.concurrent.Executors

import concurrent.ExecutionContext
import concurrent.duration._

import cats.effect.{ContextShift, ExitCode, IO, Timer}

import org.http4s.{Charset, HttpRoutes, MediaType, StaticFile}
import org.http4s.dsl.io._
import org.http4s.headers.`Content-Type`
import org.http4s.server.ServerBuilder
import org.http4s.server.blaze.BlazeServerBuilder

import model.Node

/**
 * The application that serves an oversite website.
 */
object Server {

  /** The value to use for the default host. */
  val DefaultHost: String = ""

  /** The value to use for the default port. */
  val DefaultPort: Int = -1

  /** The value to use for the default shutdown wait time. */
  val DefaultWait: Duration = 2.seconds

  /**
   * Runs the server indefinitely.
   *
   * @param root  The root node in the site.
   * @param host  The host to bind to.
   * @param port  The port to bind to.
   * @param wait  The duration to wait for the server to shut down.
   * @param shift The context to shift from.
   * @param timer The global timer.
   * @return The result of running the server.
   */
  def apply(
    root: Node.Root[_ <: AnyRef],
    host: String = DefaultHost,
    port: Int = DefaultPort,
    wait: Duration = DefaultWait
  )(implicit
    shift: ContextShift[IO],
    timer: Timer[IO]
  ): IO[ExitCode] =
    IO(ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())).bracket {
      blockingExecutionContext =>
        BlazeServerBuilder[IO].bindHttp(
          if (port < 0) ServerBuilder.DefaultHttpPort else port,
          if (host.isEmpty) ServerBuilder.DefaultHost else host
        ).withHttpApp(HttpRoutes.of[IO] {
          case request@GET -> path =>
            redeeming {
              redeeming(root.resolve(Pointer.parse(path.toList.mkString("/", "/", "")))) {
                case Pointer.Target(tpe, prefix, suffix) =>
                  root.index flatMap { index =>
                    prefix.toLocation(root.location) flatMap (index(_)) map { node =>
                      tpe match {
                        case _: Pointer.Asset =>
                          redeeming(node.asInstanceOf[Node.Parent[_]].readAsset(suffix.toString)) {
                            case Left(data) => suffix.toString lastIndexOf '.' match {
                              case i if i >= 0 => MediaType forExtension suffix.toString.substring(i + 1) map { media =>
                                Ok(data) map (_.withContentType(`Content-Type`(media)))
                              } getOrElse NotFound()
                              case _ => MediaType forExtension Pointer.Page.html.extension.normal map { media =>
                                Ok(data) map (_.withContentType(`Content-Type`(media)))
                              } getOrElse NotFound()
                            }
                            case Right(url) =>
                              StaticFile.fromURL(url, blockingExecutionContext, Some(request)) getOrElseF NotFound()
                          }(_ => NotFound())
                        case _ => node.read() flatMap { published =>
                          Ok(published getBytes "UTF-8")
                            .map(_.withContentType(`Content-Type`(MediaType.all("text" -> "html"), Charset.`UTF-8`)))
                        }
                      }
                    } getOrElse NotFound()
                  }
                case _ => NotFound()
              }(_ => NotFound())
            }(IO.pure)(msg => InternalServerError(msg))
        }.orNotFound).serve.compile.lastOrError
    }(blockingExecutionContext => IO({
      blockingExecutionContext.shutdown()
      blockingExecutionContext.awaitTermination(wait.length, wait.unit)
    }))

  private def redeeming[T, U](op: IO[T])(continue: T => IO[U])(redemption: String => IO[U]): IO[U] =
    op.redeemWith(
      t => for {
        _ <- IO(t.printStackTrace()).redeem(_ => (), identity)
        result <- redemption(
          (s"${t.getClass.getName}: ${t.getMessage}" +: t.getStackTrace.map("  " + _.toString)) mkString "\r\n"
        )
      } yield result,
      continue
    )

}
