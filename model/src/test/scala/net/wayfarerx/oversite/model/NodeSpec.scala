/*
 * NodeSpec.scala
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

import java.net.URLClassLoader
import java.nio.file.Paths

import cats.effect.IO

import org.scalatest.{FlatSpec, Matchers}

/**
 * Test suite for the node implementations.
 */
class NodeSpec extends FlatSpec with Matchers {

  import NodeSpec._

  private val site = new SiteMap

  "Node" should "load valid sites" in {
    site.root.entity.unsafeRunSync() shouldBe Home(
      name"root-document",
      Some(Author(name"wayfarerx", None, None)),
      Vector(Markup.Text("The root document."))
    )
    site.root.children.unsafeRunSync().toSet shouldBe Set(site.test1, site.test2, site.aliased)
    site.test1.name shouldBe name"test-1"
    site.test1.entity.unsafeRunSync() shouldBe Test1(
      name"test-document-1",
      Some(Author(name"wayfarerx", None, None)),
      Vector(Markup.Text("The first test document."))
    )
    site.test1.children.unsafeRunSync().toSet shouldBe Set(site.test1Nested)
    site.test1Nested.name shouldBe name"nested"
    site.test1Nested.entity.unsafeRunSync() shouldBe Test1(
      name"nested-test-document-1",
      Some(Author(name"wayfarerx", None, None)),
      Vector(Markup.Text("The nested first test document."))
    )
    site.test2.name shouldBe name"test-2"
    site.test2.entity.unsafeRunSync() shouldBe Test2(
      name"test-document-2",
      Some(Author(name"wayfarerx", None, None)),
      Vector(Markup.Text("The second test document."))
    )
    site.test2.children.unsafeRunSync().toSet shouldBe Set(site.test2Nested, site.deep)
    site.test2Nested.name shouldBe name"nested"
    site.test2Nested.entity.unsafeRunSync() shouldBe Test2Nested(
      name"nested-test-document-2",
      Some(Author(name"wayfarerx", None, None)),
      Vector(Markup.Text("The nested second test document."))
    )
    site.deep.children.unsafeRunSync() shouldBe Vector.empty
    site.deep.name shouldBe name"deep"
    site.deep.entity.unsafeRunSync() shouldBe Test2(
      name"deep-document",
      Some(Author(name"wayfarerx", None, None)),
      Vector(Markup.Text("The deep document."))
    )
    site.deep.site.isInstanceOf[TestSite] shouldBe true

    site.aliased.name shouldBe name"aliased"
    site.aliased.entity.unsafeRunSync() shouldBe Aliased(
      name"aliased-document",
      Some(Author(name"wayfarerx", None, None)),
      Vector(Markup.Text("The aliased document."))
    )
    site.aliased.children.unsafeRunSync() shouldBe Vector.empty
    site.aliased.site.isInstanceOf[TestSite] shouldBe true
  }

  it should "load the titles of each page" in {
    site.root.title.unsafeRunSync() shouldBe name"Root Document"
    site.test1.title.unsafeRunSync() shouldBe name"Test Document 1"
    site.test1Nested.title.unsafeRunSync() shouldBe name"Nested Test Document 1"
    site.test2.title.unsafeRunSync() shouldBe name"Test Document 2"
    site.test2Nested.title.unsafeRunSync() shouldBe name"Nested Test Document 2"
    site.deep.title.unsafeRunSync() shouldBe name"Deep Document"
  }

  it should "resolve author references" in {
    site.root.resolve(Author(name"nobody")).unsafeRunSync() shouldBe
      Author(name"nobody", None, None)
    site.test1.resolve(Author(name"wayfarerx")).unsafeRunSync() shouldBe
      Author(name"wayfarerx", Some("thewayfarerx"), Some("x@wayfarerx.net"))
  }

  it should "resolve entity pointers" in {
    site.root.resolve(Pointer.Entity[Home].apply(Path.empty)).unsafeRunSync() shouldBe
      Pointer.Entity[Home].apply(Path.empty)
    site.root.resolve(Pointer.External(Pointer.Page, "http://example.com/"))
      .unsafeRunSync() shouldBe Pointer.External(Pointer.Page, "http://example.com/")
    site.root.resolve(Pointer.External(Pointer.Page, "http://example.com/"): Pointer[Pointer.Page])
      .unsafeRunSync() shouldBe Pointer.External(Pointer.Page, "http://example.com/")
    site.root.resolve[Pointer.Entity[Test1], Unit](Pointer.Entity[Test1].apply(name"test-1"))
      .unsafeRunSync() shouldBe Pointer.Entity[Test1].apply(Path("test-1"))
    a[Node.Problem] should be thrownBy
      site.root.resolve(Pointer.Entity[Test1].apply(name"not-there")).unsafeRunSync()
    a[Node.Problem] should be thrownBy
      site.root.resolve(Pointer.Entity[Test1].apply(Path("not-there"))).unsafeRunSync()
  }

  it should "resolve image pointers" in {
    site.root.resolve(Pointer.Image(name"wx-gear"))
      .unsafeRunSync() shouldBe Pointer.Image(Path.empty, "images/wx-gear.png")
    site.test1.resolve(Pointer.Image(name"wx-gear"))
      .unsafeRunSync() shouldBe Pointer.Image(Location.empty, "images/wx-gear.png")
    site.aliased.resolve(Pointer.Image(name"wx-gear"))
      .unsafeRunSync() shouldBe Pointer.Image(Location.empty, "images/wx-gear.png")
    a[Node.Problem] should be thrownBy site.root.resolve(Pointer.Image(name"wx-gear-x3"))
      .unsafeRunSync()
    site.test1.resolve(Pointer.Image(name"wx-gear-x3"))
      .unsafeRunSync() shouldBe Pointer.Image(Path.empty, "images/wx-gear-x3.png")
    site.aliased.resolve(Pointer.Image(name"wx-gear-x3"))
      .unsafeRunSync() shouldBe Pointer.Image(Path.empty, "images/wx-gear-x3.png")
    site.root.resolve(Pointer.Image(Path.empty, "images/wx-gear.png"))
      .unsafeRunSync() shouldBe Pointer.Image(Path.empty, "images/wx-gear.png")
    site.test1.resolve(Pointer.Image(Location.empty, "images/wx-gear.png"))
      .unsafeRunSync() shouldBe Pointer.Image(Location.empty, "images/wx-gear.png")
    site.aliased.resolve(Pointer.Image(Location.empty, "images/wx-gear.png"))
      .unsafeRunSync() shouldBe Pointer.Image(Location.empty, "images/wx-gear.png")
    a[Node.Problem] should be thrownBy site.root.resolve(Pointer.Image(Path.empty, "images/wx-gear-x3.png"))
      .unsafeRunSync()
    site.test1.resolve(Pointer.Image(Path.empty, "images/wx-gear-x3.png"))
      .unsafeRunSync() shouldBe Pointer.Image(Path.empty, "images/wx-gear-x3.png")
    site.aliased.resolve(Pointer.Image(Path.empty, "images/wx-gear-x3.png"))
      .unsafeRunSync() shouldBe Pointer.Image(Path.empty, "images/wx-gear-x3.png")

  }

  it should "resolve page pointers" in {
    site.root.resolve(Pointer.Page(name"page"))
      .unsafeRunSync() shouldBe Pointer.Page(Path.empty, "page.html")
    site.test1.resolve(Pointer.Page(name"page"))
      .unsafeRunSync() shouldBe Pointer.Page(Location.empty, "page.html")
    a[Node.Problem] should be thrownBy site.root.resolve(Pointer.Page(name"not-a-page"))
      .unsafeRunSync()
  }

  it should "resolve generated pointers" in {
    site.test2.resolve(Pointer.Stylesheet(name"generated"))
      .unsafeRunSync() shouldBe Pointer.Stylesheet(Path.empty, "stylesheets/generated.css")
    site.root.resolve(Pointer.Stylesheet("test-2/stylesheets/generated.css"))
      .unsafeRunSync() shouldBe Pointer.Stylesheet(Path("test-2"), "stylesheets/generated.css")
    site.test2.resolve(Pointer.Stylesheet("../stylesheets/styles.css"))
      .unsafeRunSync() shouldBe Pointer.Stylesheet(Location.empty, "stylesheets/styles.css")
    site.root.resolve(Pointer.Stylesheet("stylesheets/styles.css"))
      .unsafeRunSync() shouldBe Pointer.Stylesheet(Path.empty, "stylesheets/styles.css")
    site.test2.generates(Path("foo/bar"), "styles.css") shouldBe false
    site.test2.generates(Path("foo"), "bar") shouldBe false
    site.test2.generates(Path("foo"), "bar.css") shouldBe false
  }

  it should "search for entities" in {
    site.root.search[Home]().unsafeRunSync() shouldBe
      Vector(Pointer.Entity[Home].apply(Path.empty))
    site.root.search[Test1]().unsafeRunSync() shouldBe
      Vector(Pointer.Entity[Test1].apply(Path("test-1")), Pointer.Entity[Test1].apply(Path("test-1/nested")))
    site.root.search[Test2]().unsafeRunSync() shouldBe
      Vector(Pointer.Entity[Test2].apply(Path("test-2")), Pointer.Entity[Test2].apply(Path("test-2/deep")))
    site.root.search[Test2Nested]().unsafeRunSync() shouldBe
      Vector(Pointer.Entity[Test2Nested].apply(Path("test-2/nested")))
    site.root.search[Aliased]().unsafeRunSync() shouldBe
      Vector(Pointer.Entity[Aliased].apply(Path("aliased")))
    val all = site.root.search[Model]().unsafeRunSync()
    all.length shouldBe 7
    all.head shouldBe Pointer.Entity[Model].apply(Path.empty)
    all.slice(1, 4) sortBy (_.href) shouldBe Vector(
      Pointer.Entity[Model].apply(Path("aliased")),
      Pointer.Entity[Model].apply(Path("test-1")),
      Pointer.Entity[Model].apply(Path("test-2"))
    )
    all.slice(4, 7) sortBy (_.href) shouldBe Vector(
      Pointer.Entity[Model].apply(Path("test-1/nested")),
      Pointer.Entity[Model].apply(Path("test-2/deep")),
      Pointer.Entity[Model].apply(Path("test-2/nested"))
    )
  }

  it should "search for entities by their relations" in {
    val test1 = Query[Home](Home.tests, Pointer.Entity[Test1].apply(Path("test-1")))
    val test2 = Query[Home](Home.tests, Pointer.Entity[Test2].apply(Path("test-2")))
    val aliased = Query[Home](Home.tests, Pointer.Entity[Aliased].apply(Path("aliased")))
    site.root.search(test1).unsafeRunSync() shouldBe Vector(Pointer.Entity[Home].apply(Path.empty))
    site.root.search(test2).unsafeRunSync() shouldBe Vector(Pointer.Entity[Home].apply(Path.empty))
    site.root.search(aliased).unsafeRunSync() shouldBe Vector.empty
    site.root.search(!test1).unsafeRunSync() shouldBe Vector.empty
    site.root.search(!test2).unsafeRunSync() shouldBe Vector.empty
    site.root.search(!aliased).unsafeRunSync() shouldBe Vector(Pointer.Entity[Home].apply(Path.empty))
    site.root.search(test1 & test2).unsafeRunSync() shouldBe Vector(Pointer.Entity[Home].apply(Path.empty))
    site.root.search(test1 & aliased).unsafeRunSync() shouldBe Vector.empty
    site.root.search(test1 | test2).unsafeRunSync() shouldBe Vector(Pointer.Entity[Home].apply(Path.empty))
    site.root.search(test1 | aliased).unsafeRunSync() shouldBe Vector(Pointer.Entity[Home].apply(Path.empty))
    site.root.search(test1 ^ test2).unsafeRunSync() shouldBe Vector.empty
    site.root.search(test1 ^ aliased).unsafeRunSync() shouldBe Vector(Pointer.Entity[Home].apply(Path.empty))
  }

  it should "describe entities" in {
    site.root.describe(site.root.self).unsafeRunSync() shouldBe
      Metadata(
        name"Root Document",
        Some(Author(name"wayfarerx")),
        Vector(Markup.Text("The root document.")),
        Some(Pointer.Image(Pointer.Image.name))
      )
    site.root.describe(Pointer.Entity[Test1].apply(name"test-1")).unsafeRunSync() shouldBe
      Metadata(
        name"Test Document 1",
        Some(Author(name"wayfarerx")),
        Vector(Markup.Text("The first test document.")),
        Some(Pointer.Image(Pointer.Image.name))
      )
    site.root.describe(Pointer.Entity[Test2].apply(Location.resolved(Path("test-2")))).unsafeRunSync() shouldBe
      Metadata(
        name"Test Document 2",
        Some(Author(name"wayfarerx")),
        Vector(Markup.Text("The second test document.")),
        Some(Pointer.Image(Pointer.Image.name))
      )
  }

  it should "load entities" in {
    site.root.load(Pointer.Entity[Home].apply(Location.empty)).unsafeRunSync() shouldBe Home(
      name"Root Document",
      Some(Author(name"wayfarerx")),
      Vector(Markup.Text("The root document."))
    )
    site.root.load(Pointer.Entity[Test1].apply(name"test-1")).unsafeRunSync() shouldBe Test1(
      name"Test Document 1",
      Some(Author(name"wayfarerx")),
      Vector(Markup.Text("The first test document."))
    )
    site.root.load(Pointer.Entity[Test2].apply(Path("test-2"))).unsafeRunSync() shouldBe Test2(
      name"Test Document 2",
      Some(Author(name"wayfarerx")),
      Vector(Markup.Text("The second test document."))
    )
  }

  it should "load alt text" in {
    site.root.alt(Pointer.Image(name"wx-gear")).unsafeRunSync() shouldBe Some("The wayfarerx gear.")
    site.root.alt(Pointer.Image("images/wx-gear.png")).unsafeRunSync() shouldBe Some("The wayfarerx gear.")
    site.test1.alt(Pointer.Image(name"wx-gear-x3")).unsafeRunSync() shouldBe None
    site.test1.alt(Pointer.Image("images/wx-gear-x3.png")).unsafeRunSync() shouldBe None
    site.test1.alt(Pointer.External(Pointer.Image, "http://example.com/image.gif")).unsafeRunSync() shouldBe None
    a[Node.Problem] should be thrownBy site.root.alt(Pointer.Image(name"missing")).unsafeRunSync()
    a[Node.Problem] should be thrownBy site.root.alt(Pointer.Image("images/missing.png")).unsafeRunSync()
  }

}

/**
 * Fixtures used in the node test suite.
 */
object NodeSpec {

  /**
   * A mapping of the expected resources in a site.
   */
  private final class SiteMap {

    /** The root node. */
    lazy val root = {
      val file = Paths.get("model/src/test/resources/node-spec/site").toUri.toURL
      val resources = Resources.Classpath(new URLClassLoader(Array(file), getClass.getClassLoader))
      Node.Root[Home](classOf[TestSite].getName, resources).unsafeRunSync()
    }

    /** The test-1 node. */
    lazy val test1 =
      root.children.unsafeRunSync().find(_.name == name"test-1").get.asInstanceOf[Node.Branch[_ <: AnyRef]]

    /** The test-1/nested node. */
    lazy val test1Nested =
      test1.children.unsafeRunSync().find(_.name == name"nested").get.asInstanceOf[Node.Leaf[_ <: AnyRef]]

    /** The test-2 node. */
    lazy val test2 =
      root.children.unsafeRunSync().find(_.name == name"test-2").get.asInstanceOf[Node.Branch[_ <: AnyRef]]

    /** The test-2/nested node. */
    lazy val test2Nested =
      test2.children.unsafeRunSync().find(_.name == name"nested").get.asInstanceOf[Node.Leaf[_ <: AnyRef]]

    /** The test-2/deep node. */
    lazy val deep =
      test2.children.unsafeRunSync().find(_.name == name"deep").get.asInstanceOf[Node.Branch[_ <: AnyRef]]

    /** The aliased node. */
    lazy val aliased =
      root.children.unsafeRunSync().find(_.name == name"aliased").get.asInstanceOf[Node.Branch[_ <: AnyRef]]

  }

  /** Base type for publishable fixtures. */
  sealed trait Model

  /** Provides the publisher for all model objects. */
  object Model {

    implicit def publisher[T <: Model]: Publisher[T] = new Publisher[T] {
      override def publish(entity: T)(implicit ctx: Context): IO[String] = IO(entity.toString)
    }

    def decoder[T <: AnyRef](f: (Name, Option[Author], Vector[Markup.Inline]) => T): Decoder[T] = new Decoder[T] {
      override def decode(document: Document)(implicit ctx: Context): IO[T] =
        IO.pure(f(document.metadata.name, document.metadata.author, document.metadata.description))
    }

  }

  /** The home entity. */
  case class Home(name: Name, author: Option[Author], description: Vector[Markup.Inline]) extends Model

  /** Provides the decoder for home entities. */
  object Home {

    implicit val decoder: Decoder[Home] = Model.decoder { case (n, a, d) => Home(n, a, d) }

    val tests: Relation[Home] = Relation(_ => Set(
      Pointer.Entity[Model].apply(Path("test-1")),
      Pointer.Entity[Model].apply(Path("test-2"))
    ))

  }

  /** The test-1 entity. */
  case class Test1(name: Name, author: Option[Author], description: Vector[Markup.Inline]) extends Model

  /** Provides the decoder for test-1 entities. */
  object Test1 {
    implicit val decoder: Decoder[Test1] = Model.decoder { case (n, a, d) => Test1(n, a, d) }
  }

  /** The test-2 entity. */
  case class Test2(name: Name, author: Option[Author], description: Vector[Markup.Inline]) extends Model

  /** Provides the decoder for test-2 entities. */
  object Test2 {
    implicit val decoder: Decoder[Test2] = Model.decoder { case (n, a, d) => Test2(n, a, d) }
  }

  /** The test-2 nested entity. */
  case class Test2Nested(name: Name, author: Option[Author], description: Vector[Markup.Inline]) extends Model

  /** Provides the decoder for test-2 nested entities. */
  object Test2Nested {
    implicit val decoder: Decoder[Test2Nested] = Model.decoder { case (n, a, d) => Test2Nested(n, a, d) }
  }

  /** The test-2 nested entity. */
  case class Aliased(name: Name, author: Option[Author], description: Vector[Markup.Inline]) extends Model

  /** Provides the decoder for test-2 nested entities. */
  object Aliased {
    implicit val decoder: Decoder[Aliased] = Model.decoder { case (n, a, d) => Aliased(n, a, d) }
  }

  /** The site fixture. */
  final class TestSite extends Site[Home] {

    override def name: Name = name"Test"

    override def owner: Author = Author(name"wayfarerx")

    override def baseUrl: String = "http://example.com"

    override def scopes: Scope[Home] = Scope[Home](
      Scope.Select(name"test-1") -> Scope[Test1](),
      Scope.Select(name"test-2") -> Scope[Test2](
        Vector(Scope.Generator(
          name"generated",
          Pointer.Stylesheet.css,
          _ => IO.pure("img { border: 0; }".getBytes("UTF-8"))
        )),
        Scope.Select(name"nested") -> Scope[Test2Nested]()
      ),
      Scope.Select(name"aliased") -> Scope.Aliased[Aliased](Path("aliased/resources"))
    )

  }

}
