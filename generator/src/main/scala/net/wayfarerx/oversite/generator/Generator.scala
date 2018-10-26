/*
 * Generator.scala
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
package generator

import java.nio.file.{Files, Path}

import collection.immutable.ListMap

import cats.effect.IO

import fs2.{Stream, io, text}

import model._

/**
 * A utility for generating all the resources in a site.
 */
object Generator {

  /**
   * Generates the website at the specified target.
   *
   * @tparam T The type of source to load the site from.
   * @param site   The source to load a site from.
   * @param target The target to write the site to.
   * @return The paths that were written.
   */
  def apply[T: Website.Source](site: T, target: Path): IO[Vector[Path]] =
    IO(new model.Environment(Thread.currentThread.getContextClassLoader)) flatMap { implicit env =>
      for {
        website <- Website(site)
        _ <- IO(Files.createDirectories(target))
        paths <- generate(target, Vector(website.root))
      } yield paths.sortBy(_.toString)
    }

  /**
   * Generates the static content for the specified pages.
   *
   * @param parent The directory that contains the pages.
   * @param remaining The pages that remain to be generated.
   * @param env The environment to operate in.
   * @return The paths of all the generated files.
   */
  private def generate(
    parent: Path,
    remaining: Vector[Page[_ <: AnyRef]]
  )(
    implicit env: Environment
  ): IO[Vector[Path]] = remaining match {
    case head +: tail =>
      println(s"PAGE: ${head.location}")
      for {
      dir <- head.name map (n => IO(Files.createDirectories(parent.resolve(n.normal)))) getOrElse IO.pure(parent)
      html <- IO(dir.resolve("index.html")).flatMap(f => head.publish flatMap (write(_, f)))
      css <- IO(dir.resolve(Asset.Stylesheet.prefix)).flatMap(d => IO(Files.createDirectories(d)))
        .flatMap(deployCss(_, head.scope.stylesheets collect { case styles: Styles.Generated => styles }))
      children <- head.children
      assets <- deployAssets(s"${head.location.path}", dir, children.map(_.childName.normal + "/").toSet)
      childFiles <- generate(dir, children)
      continue <- generate(parent, tail)
    } yield {
        println(s"DIR: $dir")
        println(s"CSS: $css")
        println(s"ASSETS: $assets")
        println()
        html +: css ++: assets ++: childFiles ++: continue
      }
    case _ => IO.pure(Vector.empty)
  }

  /**
   * Deploys the assets in the specified directory and all of its child assets.
   *
   * @param directory The directory to deploy from.
   * @param target The target to deploy to.
   * @param ignored The direct child directory names to ignore.
   * @param env The environment to operate in.
   * @return The paths of all the deployed files.
   */
  private def deployAssets(
    directory: String,
    target: Path,
    ignored: Set[String]
  )(
    implicit env: Environment
  ): IO[Vector[Path]] = {

    /* Process the direct child files of the directory. */
    def processFiles(remaining: Vector[String]): IO[Vector[Path]] = remaining match {
      case head +: tail => for {
        url <- env.find(directory + head)
        file <- url map (_ => IO(Some(target.resolve(head)))) getOrElse IO.pure(None)
        _ <- (for (u <- url; f <- file) yield {
          io.readInputStream(IO(u.openStream()), 2048).through(io.file.writeAll(f)).compile.drain
        }) getOrElse IO.unit
        continue <- processFiles(tail)
      } yield file.toVector ++ continue
      case _ => IO.pure(Vector.empty)
    }

    /* Process the direct child directories of the directory. */
    def processDirectories(remaining: Vector[String]): IO[Vector[Path]] = remaining match {
      case head +: tail => for {
        nested <- IO(Files.createDirectories(target.resolve(head)))
        nestedFiles <- deployAssets(directory + head, nested, Set.empty)
        continue <- processDirectories(tail)
      } yield nestedFiles ++ continue
      case _ => IO.pure(Vector.empty)
    }

    env.list(directory) flatMap { children =>
      val (directories, files) = children.partition(_ endsWith "/")
      for {
        files <- processFiles(files filter {
          case "alt.properties" => false
          case "authors.txt" if directory.isEmpty => false
          case f if f.endsWith(".md") => false
          case _ => true
        })
        nestedFiles <- processDirectories(directories filterNot ignored)
      } yield files ++ nestedFiles
    }
  }

  /**
   * Deploys the generated CSS files to the specified directory.
   *
   * @param directory The directory to deploy to.
   * @param remaining The files remaining to deploy.
   * @return The paths of all the deployed files.
   */
  private def deployCss(directory: Path, remaining: Vector[Styles.Generated]): IO[Vector[Path]] = remaining match {
    case head +: tail => for {
      file <- IO(directory.resolve(head.name.normal + ".css")) flatMap (write(head.generate(), _))
      continue <- deployCss(directory, tail)
    } yield file +: continue
    case _ => IO.pure(Vector.empty)
  }

  /**
   * Writes a UTF-8 file.
   *
   * @param data The text to write.
   * @param file The file to write to.
   * @return The path of the deployed file.
   */
  private def write(data: String, file: Path): IO[Path] =
    Stream.eval(IO.pure(data)).through(text.utf8Encode).through(io.file.writeAll(file)).compile.drain map (_ => file)

}
