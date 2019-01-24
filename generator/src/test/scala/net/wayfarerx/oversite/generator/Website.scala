package net.wayfarerx.oversite
package generator

import cats.effect.{ExitCode, IO, IOApp}

final class Website extends Site[Home] {

  override def name: Name = name"Test Site"

  override def owner: Author = Author(name"wayfarerx")

  override def baseUrl: String = "http://localhost:8080"

  override def scopes: Scope[Home] = Scope.Aliased[Home](
    Path("net/wayfarerx/oversite/generator"),
    Styles.generator :: HomeStyles.generator :: Nil
  )

}

object Website extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = Main.run(args)

}