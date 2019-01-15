/*
 * ScopeSpec.scala
 *
 * Copyright 2018-2019 wayfarerx <x@wayfarerx.net> (@thewayfarerx)
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

  it should "provide nested scopes" in {
    val name1 = name"name1"
    val name2 = name"name2"
    val name3 = name"name3"
    val name4 = name"name4"
    val generator1 = Scope.Generator(name1, Pointer.Stylesheet.css, _ => IO.pure(Array()))
    val generator2 = Scope.Generator(name2, Pointer.Stylesheet.css, _ => IO.pure(Array()))
    val scope1 = Scope[String]()
    val scope2 = Scope[String](scope1)
    val scope3 = Scope[String](indexed = false, Scope.Select() -> scope1)
    val scope4 = Scope[String](indexed = false, scope1)
    val scope5 = Scope[String](List(generator1), Scope.Select.Matching(name4) -> scope1)
    val scope6 = Scope[String](List(generator2), scope1)
    val scope7 = Scope[String](indexed = false, List(generator1))
    val scope8 = Scope[String](indexed = false, List(generator2), scope1)
    scope1 shouldBe Scope.Nested(indexed = true, List.empty, List.empty)
    scope2 shouldBe Scope.Nested(indexed = true, List.empty, List(Scope.Select.All -> scope1))
    scope3 shouldBe Scope.Nested(indexed = false, List.empty, List(Scope.Select.All -> scope1))
    scope4 shouldBe Scope.Nested(indexed = false, List.empty, List(Scope.Select.All -> scope1))
    scope5 shouldBe Scope.Nested(indexed = true, List(generator1), List(Scope.Select(name4) -> scope1))
    scope6 shouldBe Scope.Nested(indexed = true, List(generator2), List(Scope.Select.All -> scope1))
    scope7 shouldBe Scope.Nested(indexed = false, List(generator1), List.empty)
    scope8 shouldBe Scope.Nested(indexed = false, List(generator2), List(Scope.Select.All -> scope1))
    scope1(name1) shouldBe scope1
    scope1(name2) shouldBe scope1
    scope2(name1) shouldBe scope1
    scope2(name2) shouldBe scope1
    scope3(name1) shouldBe scope1
    scope3(name2) shouldBe scope1
    scope3(name3) shouldBe scope1
    scope4(name1) shouldBe scope1
    scope4(name2) shouldBe scope1
    scope4(name3) shouldBe scope1
  }

  it should "provide aliased scopes" in {
    val path = Path("path")
    val name1 = name"name1"
    val name2 = name"name2"
    val name3 = name"name3"
    val name4 = name"name4"
    val generator1 = Scope.Generator(name1, Pointer.Stylesheet.css, _ => IO.pure(Array()))
    val generator2 = Scope.Generator(name2, Pointer.Stylesheet.css, _ => IO.pure(Array()))
    val scope1 = Scope.Aliased[String](path)
    val scope2 = Scope.Aliased[String](path, scope1)
    val scope3 = Scope.Aliased[String](path, indexed = false, Scope.Select() -> scope1)
    val scope4 = Scope.Aliased[String](path, indexed = false, scope1)
    val scope5 = Scope.Aliased[String](path, List(generator1), Scope.Select.Matching(name4) -> scope1)
    val scope6 = Scope.Aliased[String](path, List(generator2), scope1)
    val scope7 = Scope.Aliased[String](path, indexed = false, List(generator1))
    val scope8 = Scope.Aliased[String](path, indexed = false, List(generator2), scope1)
    scope1 shouldBe Scope.Aliased(path, indexed = true, List.empty, List.empty)
    scope2 shouldBe Scope.Aliased(path, indexed = true, List.empty, List(Scope.Select.All -> scope1))
    scope3 shouldBe Scope.Aliased(path, indexed = false, List.empty, List(Scope.Select.All -> scope1))
    scope4 shouldBe Scope.Aliased(path, indexed = false, List.empty, List(Scope.Select.All -> scope1))
    scope5 shouldBe Scope.Aliased(path, indexed = true, List(generator1), List(Scope.Select(name4) -> scope1))
    scope6 shouldBe Scope.Aliased(path, indexed = true, List(generator2), List(Scope.Select.All -> scope1))
    scope7 shouldBe Scope.Aliased(path, indexed = false, List(generator1), List.empty)
    scope8 shouldBe Scope.Aliased(path, indexed = false, List(generator2), List(Scope.Select.All -> scope1))
    val extended = Scope.Nested(scope1.indexed, scope1.generators, scope1.children)
    scope1(name1) shouldBe extended
    scope1(name2) shouldBe extended
    scope2(name1) shouldBe scope1
    scope2(name2) shouldBe scope1
    scope3(name1) shouldBe scope1
    scope3(name2) shouldBe scope1
    scope3(name3) shouldBe scope1
    scope4(name1) shouldBe scope1
    scope4(name2) shouldBe scope1
    scope4(name3) shouldBe scope1
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
