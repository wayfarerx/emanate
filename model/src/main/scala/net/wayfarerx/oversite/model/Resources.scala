/*
 * Resources.scala
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

import java.net.{URL, URLClassLoader}
import java.nio.file.{Files, Paths, Path => JPath}
import java.util.jar.JarFile

import collection.JavaConverters._
import cats.effect.IO

/**
 * Base type for all resource collections.
 */
trait Resources {

  /**
   * Attempts to return the URL of a specific resource if it exists.
   *
   * @param resource The name of the resource to look up.
   * @return The result of attempting to return the URL of a specific resource if it exists.
   */
  final def find(resource: Path.Regular): IO[Option[URL]] =
    search(resource) map (_.headOption)

  /**
   * Looks up the URLs provided for a specific resource.
   *
   * @param resource The name of the resource to look up.
   * @return The URLs provided for a specific resource.
   */
  def search(resource: Path.Regular): IO[Vector[URL]]

  /**
   * Attempts to list the resources contained in a directory.
   *
   * @param directory The directory to list the immediate children of.
   * @return The result of attempting to list the resources contained in a directory.
   */
  def list(directory: Path.Regular): IO[Vector[String]]

  /**
   * Attempts to load a class from this collection of resources.
   *
   * @param className The name of the class to load.
   * @return  The result of attempting to load a class from this collection of resources.
   */
  def load(className: String): IO[Class[_]]

}

/**
 * Definitions of the provided resource collections.
 */
object Resources {

  /**
   * The resources available on the classpath.
   *
   * @param classLoader The URL class loader to use.
   */
  case class Classpath(classLoader: ClassLoader) extends Resources {

    import Classpath._

    /** The source to load resources from. */
    private val source = Source(classLoader)

    /* Attempt to return the URLs for a specific resource if it exists. */
    override def search(resource: Path.Regular): IO[Vector[URL]] =
      Query(resource) map source.search getOrElse IO.pure(Vector.empty)

    /* Attempt to list the resources contained in a directory. */
    override def list(directory: Path.Regular): IO[Vector[String]] =
      Query(directory) map source.list getOrElse IO.pure(Vector.empty)

    /* Attempt to load the specified class. */
    override def load(className: String): IO[Class[_]] =
      IO(classLoader.loadClass(className))

  }

  /**
   * Support for class paths.
   */
  object Classpath {

    /** The UTF-8 encoding. */
    private val UTF8 = "UTF-8"

    /**
     * A normalized, formal query for a class path.
     *
     * @param path The normalized, formal path the class path.
     */
    private class Query private(val path: String) extends AnyVal

    /**
     * Factory for normalized, formal query for a class path.
     */
    private object Query {

      /** The lower case name of the `META-INF` directory. */
      private val META_INF = "meta-inf"

      /**
       * Attempts to create a new query.
       *
       * @param path The path to query.
       * @return A new query if one can be created.
       */
      def apply(path: Path.Regular): Option[Query] = {
        val normal: Path.Regular = if (path startsWith "/") path substring 1 else path
        normal.toLowerCase match {
          case META_INF => None
          case meta if meta startsWith s"$META_INF/" => None
          case cls if cls endsWith ".class" => None
          case _ => Some(new Query(normal))
        }
      }

    }

    /**
     * Base type for sources of resources.
     */
    private sealed trait Source {

      /**
       * Searches for all resources that satisfy the specified query.
       *
       * @param query The query to satisfy.
       * @return The matching resources.
       */
      def search(query: Query): IO[Vector[URL]]

      /**
       * Lists the contents of a directory.
       *
       * @param query The query that specifies the directory.
       * @return The contents of the queried directory.
       */
      def list(query: Query): IO[Vector[String]]

    }

    /**
     * Factory for sources of resources.
     */
    private object Source {

      /**
       * Creates a source from a class loader.
       *
       * @param classLoader The class loader to use as a source.
       * @return A source for the specified class loader.
       */
      def apply(classLoader: ClassLoader): Source = classLoader match {
        case urls: URLClassLoader => URLs(urls)
        case other => Generic(other)
      }

      /**
       * Support for generic class loaders.
       *
       * @param classLoader The class loader to create a source for.
       */
      private case class Generic(classLoader: ClassLoader) extends Source {

        /* Search the class loader for resources. */
        override def search(query: Query): IO[Vector[URL]] =
          IO(classLoader.getResources(query.path)) map (_.asScala.toVector)

        /* Attempt to list the child resources. */
        override def list(query: Query): IO[Vector[String]] =
          IO(Option(classLoader.getResourceAsStream(query.path))).bracket {
            case Some(stream) =>
              IO(io.Source.fromInputStream(stream, UTF8).getLines() map (_.trim) filterNot (_.isEmpty)) map {
                _.map(line => Path.Regular(s"${query.path}/$line").string).toVector
              }
            case None =>
              IO.pure(Vector.empty)
          }(s => IO(s foreach (_.close())))

      }

      /**
       * Support for URL class loaders.
       *
       * @param classLoader The class loader to create a source for.
       */
      private case class URLs(classLoader: URLClassLoader) extends Source {

        import URLs._

        /** The parent of this source if one exists. */
        private val parent = Option(classLoader.getParent) map (Source(_))

        /** The URLs referenced by this source. */
        private val entries = Cached {

          /* Qualify all the URLs. */
          def load(remaining: Vector[URL]): IO[Vector[Entry]] = remaining match {
            case head +: tail => for {
              h <- Entry(head)
              t <- load(tail)
            } yield h map (_ +: t) getOrElse t
            case _ => IO.pure(Vector.empty)
          }

          load(classLoader.getURLs.toVector)
        }

        /* Search the URLs and then search the parent. */
        override def search(query: Query): IO[Vector[URL]] = {

          /* Search in each URL. */
          def searching(remaining: Vector[Entry]): IO[Vector[URL]] = remaining match {
            case head +: tail => for {
              h <- head.search(query)
              t <- searching(tail)
            } yield h ++ t
            case _ => IO.pure(Vector.empty)
          }

          for {
            inherited <- parent map (_.search(query)) getOrElse IO.pure(Vector.empty)
            provided <- entries() flatMap searching
          } yield inherited ++ provided
        }

        /* List from the URLs and then list from the parent. */
        override def list(query: Query): IO[Vector[String]] = {

          /* List from each URL. */
          def listing(remaining: Vector[Entry]): IO[Vector[String]] = remaining match {
            case head +: tail => for {
              h <- head.list(query)
              t <- listing(tail)
            } yield h ++ t
            case _ => IO.pure(Vector.empty)
          }

          for {
            inherited <- parent map (_.list(query)) getOrElse IO.pure(Vector.empty)
            provided <- entries() flatMap listing
          } yield inherited ++ provided
        }

      }

      /**
       * Definitions associated with URL class loaders.
       */
      private object URLs {

        /**
         * Base type for URL classpath entries that resources can be loaded from.
         */
        private sealed trait Entry {

          /**
           * Searches for all resources that satisfy the specified query.
           *
           * @param query The query to satisfy.
           * @return The matching resources.
           */
          def search(query: Query): IO[Vector[URL]]

          /**
           * Lists the contents of a directory.
           *
           * @param query The query that specifies the directory.
           * @return The contents of the queried directory.
           */
          def list(query: Query): IO[Vector[String]]

        }

        /**
         * Factory for URL classpath entries.
         */
        private object Entry {

          /**
           * Attempts to create a classpath entry for the specified URL.
           *
           * @param url The URL to create a classpath entry for.
           * @return The classpath entry if one could be created.
           */
          def apply(url: URL): IO[Option[Entry]] = if (url.getProtocol equalsIgnoreCase "file") {
            url match {
              case fileUrl if fileUrl.toExternalForm.endsWith("/") => for {
                path <- IO(Paths get fileUrl.toURI)
                directory <- IO(Files.isDirectory(path))
              } yield if (directory) Some(FileEntry(path)) else None
              case jarUrl => for {
                path <- IO(Paths get jarUrl.toURI)
                regularFile <- IO(Files.isRegularFile(path))
              } yield if (regularFile) Some(JarEntry(path)) else None
            }
          } else IO.pure(None)

          /**
           * Ensures that a path is in directory form.
           *
           * @param path The path to a directory.
           * @return The directory form of the path.
           */
          private def ensureDirectory(path: Path.Regular): Path.Regular =
            if (!path.endsWith("/")) path.string + "/" else path.string

          /**
           * File classpath entry.
           *
           * @param file The root of the classpath entry.
           */
          private case class FileEntry(file: JPath) extends Entry {

            /* Search in the file. */
            override def search(query: Query): IO[Vector[URL]] = for {
              path <- IO(file.resolve(query.path))
              exists <- IO(Files.exists(path))
              result <- if (exists) IO(Vector(path.toUri.toURL)) else IO.pure(Vector.empty)
            } yield result

            /* List in the file. */
            override def list(query: Query): IO[Vector[String]] = {

              // Recursively process results.
              def results(remaining: Vector[JPath]): IO[Vector[String]] = remaining match {
                case head +: tail => for {
                  directory <- IO(Files.isDirectory(head))
                  t <- results(tail)
                } yield Query(head.subpath(file.getNameCount, head.getNameCount).toString)
                  .map(q => if (directory) ensureDirectory(q.path).string else q.path)
                  .toVector ++ t
                case _ => IO.pure(Vector.empty)
              }

              for {
                path <- IO(file.resolve(query.path))
                directory <- IO(Files.isDirectory(path))
                result <- if (directory) {
                  IO(Files.list(path).iterator.asScala.toVector) flatMap results
                } else IO.pure(Vector.empty)
              } yield result
            }

          }

          /**
           * JAR classpath entry.
           *
           * @param jar The root of the classpath entry.
           */
          private case class JarEntry(jar: JPath) extends Entry {

            /* Search in the JAR. */
            override def search(query: Query): IO[Vector[URL]] = withJarFile { jarFile =>
              IO(jarFile.entries.asScala find { entry =>
                entry.getName == query.path || entry.getName.startsWith(ensureDirectory(query.path).string)
              }) map (_.map { entry =>
                val suffix = if (entry.getName endsWith "/") ensureDirectory(query.path).string else query.path
                new URL(s"jar:${jar.toUri.toURL}!/$suffix")
              }.toVector)
            }

            /* List in the JAR. */
            override def list(query: Query): IO[Vector[String]] = withJarFile { jarFile =>
              val prefix = ensureDirectory(query.path)
              IO(jarFile.entries.asScala.filter { entry =>
                entry.getName != prefix.string &&
                  entry.getName.startsWith(prefix) && {
                  val index = entry.getName.indexOf('/', prefix.length)
                  index < 0 || index == entry.getName.length - 1
                }
              }.map(_.getName).flatMap(Query(_)).map(_.path).toVector)
            }

            /**
             * Runs a function with a valid JAR file.
             *
             * @tparam T The return type.
             * @param f The function to run.
             * @return The result of running the function.
             */
            private def withJarFile[T](f: JarFile => IO[T]): IO[T] =
              IO(new JarFile(jar.toFile)).bracket(f)(jarFile => IO(jarFile.close()))

          }

        }

      }

    }

  }

}
