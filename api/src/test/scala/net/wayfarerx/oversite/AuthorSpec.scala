/*
 * AuthorSpec.scala
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

import org.scalatest.{FlatSpec, Matchers}

/**
 * Test suite for the author implementation.
 */
class AuthorSpec extends FlatSpec with Matchers {

  "Author" should "expose the supplied name" in {
    Name("""Foo, \$Bar./@Baz""") map { name =>
      Author(name).id shouldBe name.normal
      Author(name).toString shouldBe name.display
    } getOrElse fail("Invalid test name.")
  }

  it should "generate external links" in {
    val name = name"wayfarerx"
    Author(name).link shouldBe None
    Author(name, Some("thewayfarerx")).link shouldBe
      Some(Pointer.External(Pointer.Page, "https://twitter.com/thewayfarerx"))
    Author(name, None, Some("x@wayfarerx.net")).link shouldBe
      Some(Pointer.External(Pointer.Page, "mailto:x@wayfarerx.net"))
    Author(name, Some("thewayfarerx"), Some("x@wayfarerx.net")).link shouldBe
      Some(Pointer.External(Pointer.Page, "https://twitter.com/thewayfarerx"))
  }

}
