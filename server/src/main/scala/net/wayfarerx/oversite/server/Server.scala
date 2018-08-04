/*
 * Server.scala
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

import java.net.URL
import java.util.concurrent.{Executors, ThreadFactory, TimeUnit}
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

import concurrent.ExecutionContext

import cats.effect._
import cats.syntax.all._

import org.eclipse.jetty.server.{Server => JettyServer}
import org.eclipse.jetty.servlet.{ServletHandler, ServletHolder}

import org.apache.commons.io.IOUtils

import model._

/**
 * Main entry point for the oversite server.
 */
object Server extends IOApp {

  /** The default TCP port to use. */
  val DefaultPort: Int = 4000

  /**
   * Creates a server life cycle.
   *
   * @param className   The name of the class that implements `Site`.
   * @param port        The port to serve HTTP resources on.
   * @param environment The environment to operate in.
   * @return A new server life cycle.
   */
  def apply(className: String, port: Int = DefaultPort)(implicit environment: Environment): IO[Unit] = for {
    website <- Website(className)
    _ <- IO.shift(environment.blocking)
    _ <- IO {
      val server = new JettyServer(port)
      val handler = new ServletHandler
      handler.addServletWithMapping(new ServletHolder(new Behavior(website)), "/*")
      server.setHandler(handler)
      server.start()
      server.join()
    }
    _ <- IO.shift(environment.compute)
  } yield ()

  /**
   * Attempts to run an oversite server.
   *
   * @param args The command line arguments.
   * @return The result of the attempt to run the server.
   */
  override def run(args: List[String]): IO[ExitCode] = {
    IO(ExecutionContext.fromExecutorService(Executors.newCachedThreadPool(Threads))).bracket { blocking =>
      for {
        environment <- IO(Environment(
          Thread.currentThread.getContextClassLoader,
          ExecutionContext.global,
          blocking
        ))
        _ <- IO.shift(environment.compute)
        result <- args match {
          case className :: port :: Nil if port matches """[\d]+""" =>
            apply(className, port.toInt)(environment).as(ExitCode.Success)
          case className :: Nil =>
            apply(className)(environment).as(ExitCode.Success)
          case _ =>
            IO(println("Usage: server <site-class-name> [site-port]")).as(ExitCode(-1))
        }
      } yield result
    }(blocking => for {
      _ <- IO(blocking.shutdown())
      _ <- IO(blocking.awaitTermination(30, TimeUnit.SECONDS))
    } yield ())

  }

  /**
   * The servlet that bridges between Jetty and our internal implementation.
   *
   * @param website The website implementation.
   */
  final private class Behavior(website: Website) extends HttpServlet {

    /* Emits the requested resource if it exists. */
    override protected def doGet(request: HttpServletRequest, response: HttpServletResponse): Unit = {
      for {
        _ <- IO.shift(website.environment.compute)
        path <- IO(request.getPathInfo)
        _ <- search(path) flatMap {
          case Some(Left(page)) => publish(page, response)
          case Some(Right(url)) => publish(url, response)
          case None => notFound(path, response)
        }
      } yield ()
    }.redeemWith(t => internalError(t, response), u => IO.pure(u)).unsafeRunSync()

    /**
     * Searches the website for a page or URL.
     *
     * @param pathInfo The requested path information.
     * @return The outcome of searching the website for a page or URL.
     */
    private def search(pathInfo: String): IO[Option[Either[Page[_ <: AnyRef], URL]]] = {
      val (path, file) = pathInfo.split('/').toVector filterNot (_.isEmpty) match {
        case init :+ last if last contains '.' => Path(init flatMap (Name(_)): _*) -> Some(last)
        case all => Path(all flatMap (Name(_)): _*) -> None
      }
      file match {
        case Some(fileName) =>
          website.environment.find(s"$path/$fileName") map (_ map (Right(_)))
        case None =>
          Location(path) map (loc => website.root.index map (_ (loc)) map (_ map (Left(_)))) getOrElse IO.pure(None)
      }
    }

    /**
     * Publishes a page to a response.
     *
     * @param page     The page to publish.
     * @param response The response to publish to.
     * @return The outcome of publishing the page.
     */
    private def publish(page: Page[_ <: AnyRef], response: HttpServletResponse): IO[Unit] = for {
      html <- page.publish
      _ <- IO.shift(website.environment.blocking)
      _ <- IO(response.setContentType("text/html"))
      _ <- IO(response.getWriter).bracket(w => IO(w.write(html)))(w => IO(w.close()))
      _ <- IO.shift(website.environment.compute)
    } yield ()

    /**
     * Publishes a URL to a response.
     *
     * @param url      The URL to publish.
     * @param response The response to publish to.
     * @return The outcome of publishing the URL.
     */
    private def publish(url: URL, response: HttpServletResponse): IO[Unit] = for {
      _ <- IO.shift(website.environment.blocking)
      _ <- IO(url.openConnection()).bracket { connection =>
        for {
          contentType <- IO(connection.getContentType)
          _ <- IO(response.setContentType(contentType))
          _ <- IO(response.getOutputStream).bracket { output =>
            IO(IO(IOUtils.copy(connection.getInputStream, output)))
          }(o => IO(o.close()))
        } yield ()
      }(c => IO(c.getInputStream.close()))
      _ <- IO.shift(website.environment.compute)
    } yield ()

    /**
     * Publishes a not found message to a response.
     *
     * @param path The path that was not found.
     * @param response The response to publish to.
     * @return The outcome of publishing the not found message.
     */
    private def notFound(path: String, response: HttpServletResponse): IO[Unit] = for {
      _ <- IO.shift(website.environment.blocking)
      _ <- IO(response.setContentType("text/html"))
      _ <- IO(response.setStatus(HttpServletResponse.SC_NOT_FOUND))
      _ <- IO(response.getWriter).bracket(writer => IO(writer.write(
        s"""<html>
           |  <head>
           |    <title>404 Not Found</title>
           |  </head>
           |  <body>
           |    Not Found: $path
           |  </body>
           |</html>"""
          .stripMargin))
      )(w => IO(w.close()))
      _ <- IO.shift(website.environment.compute)
    } yield ()


    /**
     * Publishes an internal error message to a response.
     *
     * @param thrown The cause of this error report.
     * @param response The response to publish to.
     * @return The outcome of publishing the not found message.
     */
    private def internalError(thrown: Throwable, response: HttpServletResponse): IO[Unit] = for {
      _ <- IO.shift(website.environment.blocking)
      _ <- IO(response.setContentType("text/html"))
      _ <- IO(response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR))
      _ <- IO(response.getWriter).bracket(writer => IO(writer.write(
        s"""<html>
           |  <head>
           |    <title>500 Internal Server Error</title>
           |  </head>
           |  <body>
           |    ${thrown.getMessage}
           |  </body>
           |</html>"""
          .stripMargin))
      )(w => IO(w.close()))
      _ <- IO.shift(website.environment.compute)
    } yield ()

  }

  /**
   * The threads created by the server.
   */
  private object Threads extends ThreadFactory {

    /** The group that the server threads belong to. */
    val group = new ThreadGroup("oversite")

    /**
     * Creates a new server thread.
     *
     * @param runnable The action that the server is expected to perform.
     * @return The new server thread.
     */
    override def newThread(runnable: Runnable): Thread = {
      val thread = new Thread(group, runnable)
      thread.setDaemon(true)
      thread
    }

  }

}
