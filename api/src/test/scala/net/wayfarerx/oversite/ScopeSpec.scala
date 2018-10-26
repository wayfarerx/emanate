/*
 * ScopeSpec.scala
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

import reflect.ClassTag

import cats.effect.IO
import org.scalatest.{FlatSpec, Matchers}

/**
 * Test suite for the scope implementation.
 */
class ScopeSpec extends FlatSpec with Matchers {

  import ScopeSpec._

  "Scope" should "provide type information" in {
    val scope = Scope[String]()
    scope.classTag shouldBe implicitly[ClassTag[String]]
    scope.decoder shouldBe decoder
    scope.publisher shouldBe publisher
  }

  it should "provide child scopes" in {
    val name1 = name"name1"
    val name2 = name"name2"
    val name3 = name"name3"
    val scope1 = Scope[String]()
    val scope2 = Scope[String](Scope.Select(name1) -> scope1, Scope.Select() -> scope1)
    val scope3 = Scope[String](scope2)
    scope1.search(name1) shouldBe None
    scope1.search(name2) shouldBe None
    scope2.search(name1) shouldBe Some(scope1)
    scope2.search(name2) shouldBe Some(scope1)
    scope3.search(name1) shouldBe Some(scope2)
    scope3.search(name2) shouldBe Some(scope2)
    scope3.search(name3) shouldBe Some(scope2)
    scope1(name1) shouldBe scope1
    scope1(name2) shouldBe scope1
    scope2(name1) shouldBe scope1
    scope2(name2) shouldBe scope1
    scope3(name1) shouldBe scope2
    scope3(name2) shouldBe scope2
    scope3(name3) shouldBe scope2
  }

  it should "provide child stylesheets" in {
    val generated = Scope.Styles.Generated(name"generated", _ => IO.pure(""))
    val internal = Scope.Styles.Internal(Pointer.Stylesheet("stylesheet.css"))
    val external = Scope.Styles.External(Pointer.External(Pointer.Stylesheet, "https://example.com/stylesheet.css"))
    val scope1 = Scope[String](Vector(generated, internal, external))
    val scope2 = Scope[String](Vector.empty, scope1)
    scope1.stylesheets shouldBe Vector(generated, internal, external)
    scope2.stylesheets shouldBe Vector.empty
  }

}

/**
 * Definitions for the scope test suite.
 */
object ScopeSpec {

  implicit val decoder: Decoder[String] = new Decoder[String] {
    override def decode(document: Document)(implicit ctx: Context): IO[String] =
      IO.raiseError(new IllegalStateException)
  }

  implicit val publisher: Publisher[String] = new Publisher[String] {
    override def publish(entity: String)(implicit ctx: Context): IO[String] =
      IO.raiseError(new IllegalStateException)
  }

}
