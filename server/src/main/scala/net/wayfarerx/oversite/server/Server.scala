package net.wayfarerx.oversite
package server

import concurrent.ExecutionContext.Implicits.global
import cats.effect.IO
import fs2.Stream
import fs2.StreamApp.ExitCode
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.headers.`Content-Type`
import org.http4s.server.blaze._
import net.wayfarerx.oversite.{Path => OPath}

/**
 * Factory for Oversite servers.
 */
object Server {

  import model.Website

  /** The default TCP port to listen on. */
  val DefaultPort: Int = 4000

  /** The default host to listen on. */
  val DefaultHost: String = "localhost"

  /**
   * Runs a site.
   *
   * @tparam T The type of site reference to accept.
   * @param site The site to run.
   * @param port The TCP port to listen on.
   * @param host The host to listen on.
   * @return The lifecycle of the server.
   */
  def apply[T: Website.Source](site: T, port: Int = DefaultPort, host: String = DefaultHost): Stream[IO, ExitCode] =
    Stream.eval[IO, Stream[IO, ExitCode]] {
      IO(new model.Environment(Thread.currentThread.getContextClassLoader)) flatMap { implicit env =>
        Website(site) map service map (s => BlazeBuilder[IO].bindHttp(port, host).mountService(s, "/").serve)
      }
    } flatMap identity


  /**
   * Creates an HTTP service definition.
   *
   * @param website The website to serve.
   * @return The new HTTP service definition.
   */
  private def service(website: Website) = {

    /* Attempts to serve the resource at the specified path. */
    def serveResource(path: Path) =
      Some(path.toList mkString "/")
        .filterNot(_ endsWith ".md")
        .map(website.environment.find)
        .getOrElse(IO.pure(None))
        .map(_ map (url => fs2.io.readInputStream(IO(url.openStream()), 4096)))
        .flatMap {
          _ map { data =>
            path.lastOption
              .map(s => s.substring(s.lastIndexOf('.') + 1))
              .flatMap(MediaType.forExtension)
              .map(m => Ok(data, `Content-Type`(m)))
              .getOrElse(Ok(data))
          } getOrElse notFound(path)
        }

    /* Returns a not found response. */
    def notFound(path: Path) =
      NotFound(s"Not found: ${path.toList mkString "/"}")

    HttpRoutes.of[IO] {

      case GET -> path if path.lastOption exists (_ endsWith ".css") =>
        val name = path.lastOption.get.substring(0, path.lastOption.get.length - 4)
        website.site.find(path.toList.toVector.init).stylesheets collectFirst {
          case Styles.Generated(n, generate) if n.normal == name => generate
        } map { generate =>
          MediaType.forExtension("css") map (m => Ok(generate(), `Content-Type`(m))) getOrElse Ok(generate())
        } getOrElse serveResource(path)

      case GET -> path if path.lastOption exists (_ contains '.') =>
        serveResource(path)

      case GET -> path =>
        website.root.index
          .map(index => Location(OPath(path.toList.mkString("/"))) flatMap index.pagesByLocation.get)
          .flatMap(_ map (_.publish map (Some(_))) getOrElse IO.pure(None))
          .flatMap {
            _ map { page =>
              MediaType.forExtension("html") map (m => Ok(page, `Content-Type`(m))) getOrElse Ok(page)
            } getOrElse notFound(path)
          }

    }
  }

}
