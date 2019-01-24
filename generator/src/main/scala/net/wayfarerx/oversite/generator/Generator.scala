/*
 * Generator.scala
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

import java.nio.file.{Files, Path => JPath}

import cats.effect.{ContextShift, ExitCode, IO, Timer}

import org.apache.commons.io.FileUtils

import model.Node

/**
 * The application that generates an oversite website.
 */
object Generator {

  /**
   * Generates a website from the specified root node.
   *
   * @param root        The root node to generate a website from.
   * @param destination The directory to generate the website in.
   * @param shift       The context to shift from.
   * @param timer       The global timer.
   * @return The result of generating the website.
   */
  def apply(
    root: Node.Root[_ <: AnyRef],
    destination: JPath
  )(implicit
    shift: ContextShift[IO],
    timer: Timer[IO]
  ): IO[ExitCode] = {

    def visit(source: Node[_ <: AnyRef], target: JPath): IO[Unit] = for {
      _ <- deploy(source, target)
      _ <- source match {
        case parent: Node.Parent[_] => for {
          assets <- parent.listAssets()
          _ <- deployAllAssets(parent, target, assets)
          children <- parent.children
          _ <- descend(children, target)
        } yield ()
        case _ => IO.unit
      }
    } yield ()

    def deploy(source: Node[_ <: AnyRef], target: JPath): IO[Unit] = for {
      _ <- IO(Files.createDirectories(target))
      data <- source.read()
      _ <- IO(Files.write(target resolve "index.html", data getBytes "UTF-8"))
    } yield ()

    def deployAsset(parent: Node.Parent[_ <: AnyRef], target: JPath, asset: String): IO[Unit] = for {
      file <- IO(target resolve asset)
      _ <- IO(Files.createDirectories(file.getParent))
      data <- parent.readAsset(asset)
      _ <- data.fold(
        bytes => IO(Files.write(file, bytes)),
        url => IO(FileUtils.copyURLToFile(url, file.toFile))
      )
    } yield ()

    def deployAllAssets(parent: Node.Parent[_ <: AnyRef], target: JPath, remaining: List[String]): IO[Unit] =
      remaining match {
        case head :: tail =>
          deployAsset(parent, target, head) flatMap (_ => deployAllAssets(parent, target, tail))
        case _ => IO.unit
      }

    def descend(sources: List[Node.Child[_ <: AnyRef]], parent: JPath): IO[Unit] = sources match {
      case head :: tail => for {
        _ <- visit(head, parent resolve head.name.normal)
        _ <- descend(tail, parent)
      } yield ()
      case Nil => IO.unit
    }

    visit(root, destination).redeemWith(
      t => IO(t.printStackTrace()).redeem(_ => ExitCode.Error, _ => ExitCode.Error),
      _ => IO.pure(ExitCode.Success)
    )
  }

}
