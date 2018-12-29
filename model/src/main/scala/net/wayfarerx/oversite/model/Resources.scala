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
import io.Source

import cats.effect.IO

/**
 * Base type for all resource collections.
 */
trait Resources {

  /**
   * Attempts to find the URL of a specific resource if it exists.
   *
   * @param resource The name of the resource to look up.
   * @return The result of attempting to return the URL of a specific resource if it exists.
   */
  def find(resource: Path.Regular): IO[Option[URL]]

  /**
   * Attempts to detect all available URLs for a specific resource.
   *
   * @param resource The name of the resource to look up.
   * @return The URLs provided for a specific resource.
   */
  def detect(resource: Path.Regular): IO[Vector[URL]]

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
   * @return The result of attempting to load a class from this collection of resources.
   */
  def load(className: String): IO[Class[_]]

}

/**
 * Definitions of the provided resource collections.
 */
object Resources {

  /**
   * The default resource collection bound to the bootstrap class loader.
   */
  object Default extends Resources {

    /* Cannot load resources from the bootstrap class loader. */
    def find(resource: Path.Regular): IO[Option[URL]] =
      IO.pure(None)

    /* Cannot load resources from the bootstrap class loader. */
    override def detect(resource: Path.Regular): IO[Vector[URL]] =
      IO.pure(Vector())

    /* Cannot load resources from the bootstrap class loader. */
    override def list(directory: Path.Regular): IO[Vector[String]] =
      IO.pure(Vector())

    /* Load a class from the bootstrap class loader. */
    override def load(className: String): IO[Class[_]] =
      IO(Class.forName(className, true, null))

  }

  /**
   * Base type for resource collections based on class loaders.
   */
  sealed trait Classpath extends Resources {

    import Classpath._

    /** The type of class loader to use. */
    type ClassLoaderType <: ClassLoader

    /** The class loader to use. */
    def classLoader: ClassLoaderType

    /* Find resources in the underlying class loader. */
    final override def find(resource: Path.Regular): IO[Option[URL]] =
      select(resource) map (r => IO(Option(classLoader.getResource(r)))) getOrElse IO.pure(None)

    /* Detect resources in the underlying class loader. */
    final override def detect(resource: Path.Regular): IO[Vector[URL]] =
      select(resource) map (r => IO(classLoader.getResources(r).asScala.toVector)) getOrElse IO.pure(Vector())

    /* Load a class from the underlying class loader. */
    final override def load(className: String): IO[Class[_]] =
      IO(classLoader.loadClass(className))

  }

  /**
   * Definitions associated with classpaths.
   */
  object Classpath {

    /** The UTF-8 encoding. */
    private val UTF8 = "UTF-8"

    /** The lower case name of the `META-INF` directory. */
    private val META_INF = "meta-inf"

    /**
     * Creates a classpath from a class loader.
     *
     * @param classLoader The class loader to use.
     * @return The derived classpath.
     */
    def apply(classLoader: ClassLoader): Classpath = classLoader match {
      case urls: URLClassLoader => URLs(urls)
      case generic => Generic(generic)
    }

    /**
     * Selects the specified path if it is not ignored.
     *
     * @param path The path to test.
     * @return The specified path if it was selected.
     */
    private def select(path: Path.Regular): Option[Path.Regular] = {
      val normal: Path.Regular = if (path startsWith "/") path substring 1 else path
      normal.toLowerCase match {
        case META_INF => None
        case meta if meta startsWith s"$META_INF/" => None
        case cls if cls endsWith ".class" => None
        case _ => Some(normal)
      }
    }

    /**
     * The generic class loader classpath.
     *
     * @param classLoader The generic class loader to use.
     */
    case class Generic(classLoader: ClassLoader) extends Classpath {

      /* Use any class loader. */
      override type ClassLoaderType = ClassLoader

      /* Try to list resources in the underlying class loader. */
      override def list(directory: Path.Regular): IO[Vector[String]] =
        select(directory) map (Path(_)) map { path =>
          IO(Option(classLoader.getResourceAsStream(path.toString))).bracket {
            _ map { stream =>
              for {
                children <- IO(Source.fromInputStream(stream, UTF8).getLines.map(_.trim).filterNot(_.isEmpty).toVector)
                results <- (IO.pure(Vector.empty[String]) /: children) { (previous, child) =>
                  previous flatMap { collected =>
                    val resource = path + child
                    find(resource) map (_ map (_ => collected :+ resource) getOrElse collected)
                  }
                }
              } yield results
            } getOrElse IO.pure(Vector.empty)
          }(s => IO(s foreach (_.close())))
        } getOrElse IO.pure(Vector.empty)

    }

    /**
     * The URL class loader classpath.
     *
     * @param classLoader The URL class loader to use.
     */
    case class URLs(classLoader: URLClassLoader) extends Classpath {

      /* Use URL class loaders. */
      override type ClassLoaderType = URLClassLoader

      /** The parent of this URL classpath. */
      val parent: Resources = Option(classLoader.getParent) map (Classpath(_)) getOrElse Default

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
      }()

      /* Try to list resources from the underlying class loader's URLs and the parent resources. */
      override def list(directory: Path.Regular): IO[Vector[String]] =
        select(directory) map (Path(_)) map { path =>

          /* List from each URL. */
          def process(remaining: Vector[Entry]): IO[Vector[String]] = remaining match {
            case head +: tail => for {
              h <- head.list(path)
              t <- process(tail)
            } yield h ++ t
            case _ => IO.pure(Vector.empty)
          }

          for {
            inherited <- parent list directory
            provided <- entries flatMap process
          } yield inherited ++ provided
        } getOrElse IO.pure(Vector.empty)

    }

    /**
     * Base type for URL classpath entries that resources can be loaded from.
     */
    private sealed trait Entry {

      /**
       * Lists the contents of a directory.
       *
       * @param directory The path that specifies the directory.
       * @return The contents of the specified directory.
       */
      def list(directory: Path): IO[Vector[String]]

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
       * File classpath entry.
       *
       * @param file The root of the classpath entry.
       */
      private case class FileEntry(file: JPath) extends Entry {

        /* List in the file. */
        override def list(directory: Path): IO[Vector[String]] = {

          // Recursively process results.
          def results(remaining: Vector[JPath]): IO[Vector[String]] = remaining match {
            case head +: tail => results(tail) map { t =>
              head.subpath(file.getNameCount, head.getNameCount).toString.replace('\\', '/') +: t
            }
            case _ => IO.pure(Vector.empty)
          }

          for {
            path <- IO(file.resolve(directory.toString))
            okay <- IO(Files.isDirectory(path))
            result <- if (okay) {
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

        /* List in the JAR. */
        override def list(directory: Path): IO[Vector[String]] =
          IO(new JarFile(jar.toFile)).bracket { jarFile =>
            val prefix = directory.toString
            IO(jarFile.entries.asScala.filter { entry =>
              entry.getName != prefix &&
                entry.getName.startsWith(prefix) && {
                val index = entry.getName.indexOf('/', prefix.length)
                index < 0 || index == entry.getName.length - 1
              }
            }.map(_.getName).toVector)
          }(jarFile => IO(jarFile.close()))

      }

    }

  }

}
