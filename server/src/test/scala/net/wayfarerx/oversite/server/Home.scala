package net.wayfarerx.oversite
package server

import cats.effect.IO

import scalatags.Text.all._

import scalatags.text.Frag

case class Home(name: Name)

object Home {

  implicit val decoder: Decoder[Home] = new Decoder[Home] {
    override def decode(document: Document)(implicit ctx: Context): IO[Home] = IO.pure(Home(document.metadata.name))
  }

  implicit val publisher: Publisher[Home] = new ui.Template.Support[Home] {

    override def stylesheets(entity: Home)(implicit ctx: Context): IO[Vector[ui.Template.Reference]] = for {
      href <- ui.Href(Pointer.Stylesheet(HomeStyles.name))
      parent <- super.stylesheets(entity)
    } yield parent :+ ui.Template.Reference(href)

    override def display(entity: Home)(implicit ctx: Context): IO[Frag] =
      IO.pure(raw(entity.name.display))

  }

}
