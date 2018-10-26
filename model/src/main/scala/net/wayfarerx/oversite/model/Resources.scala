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

import java.net.URL
import java.nio.file.{Files, Paths, Path => JPath}
import java.util.jar.JarFile

import collection.JavaConverters._

import cats.effect.IO

/**
 * The available on the classpath.
 *
 * @param classLoader The class loader to use.
 */
final class Resources(val classLoader: ClassLoader) extends AnyVal {

  /**
   * Attempts to return the URL of a specific resource if it exists.
   *
   * @param resource The name of the resource to look up.
   * @return The result of attempting to return the URL of a specific resource if it exists.
   */
  def find(resource: String): IO[Option[URL]] = {
    val r = normalize(resource)
    if (r endsWith ".class") IO.pure(None) else IO(Option(classLoader.getResource(r)))
  }

  /**
   * Attempts to list the resources contained in a directory.
   *
   * @param directory The directory to list the immediate children of.
   * @return The result of attempting to list the resources contained in a directory.
   */
  def list(directory: String): IO[Vector[String]] = {
    val d = normalize(s"$directory/")
    for {
      target <- IO(classLoader.getResource(d)) map (Option(_))
      results <- target match {
        case Some(url) => url match {
          case fileUrl if fileUrl.getProtocol equalsIgnoreCase "file" =>
            IO(Paths get fileUrl.toURI) flatMap listInFile
          case jarUrl if jarUrl.getProtocol equalsIgnoreCase "jar" =>
            jarUrl.getPath.split(':') match {
              case Array("file", path) =>
                path.indexOf('!') match {
                  case index if index > 0 =>
                    IO(Paths.get(path take index)) flatMap (listInJar(_, d))
                  case _ =>
                    IO.raiseError(new IllegalArgumentException(s"Invalid JAR file path: $path."))
                }
              case Array(other, _) =>
                IO.raiseError(new IllegalArgumentException(s"Invalid JAR URL protocol: $other."))
              case other =>
                IO.raiseError(new IllegalArgumentException(s"Invalid JAR URL path: $other."))
            }
          case other =>
            IO.raiseError(new IllegalArgumentException(s"Unknown classpath protocol: ${other.getProtocol}."))
        }
        case None => IO.pure(Vector.empty)
      }
    } yield results filterNot (_ endsWith ".class") map (c => normalize(d + c))
  }

  /**
   * Attempts to return the paths of the resources contained in the specified directory.
   *
   * @param directory The filesystem directory to list the immediate children of.
   * @return The result of attempting to return the paths of the resources contained in the specified directory.
   */
  private def listInFile(directory: JPath): IO[Vector[String]] = for {
    isDirectory <- IO(Files.isDirectory(directory))
    children <-
      if (isDirectory) IO(Files.list(directory).iterator.asScala.toVector) flatMap { cs =>
        (IO.pure(Vector.empty[(JPath, Boolean)]) /: cs) { (results, child) =>
          results flatMap (r => IO(Files.isDirectory(child)) map (child -> _) map (r :+ _))
        }
      } else IO.raiseError(new IllegalArgumentException(s"Not a directory: $directory."))
  } yield children map {
    case (c, d) if d => c.getFileName.toString + "/"
    case (c, _) => c.getFileName.toString
  }

  /**
   * Attempts to return the paths of the resources contained in the specified JAR's directory.
   *
   * @param jarFile   The JAR file to read from.
   * @param directory The directory to list the immediate children of in the JAR.
   * @return The result of attempting to return the paths of the resources contained in the specified JAR's directory.
   */
  private def listInJar(jarFile: JPath, directory: String): IO[Vector[String]] =
    IO(new JarFile(jarFile.toFile)).bracket { jar =>
      IO {
        jar.entries().asScala
          .map(e => normalize(e.getName))
          .filter(e => e != directory && e.startsWith(directory))
          .toVector
      }
    }(jar => IO(jar.close())) map {
      _ map (_ drop directory.length) map (c => c -> c.indexOf('/')) collect {
        case (c, index) if index < 0 || index == c.length - 1 => c
      }
    }

  /**
   * Normalizes a resource path, removing initial and unnecessary slashes.
   *
   * @param path The path to normalize.
   * @return The normalized path.
   */
  private def normalize(path: String): String =
    path.dropWhile(c => c == '/' | c == '\\').replaceAll("""[\\\/]+""", "/")

}
