/*
 * AuthorsSpec.scala
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

import org.scalatest.{FlatSpec, Matchers}

/**
 * Test suite for the author data cache implementation.
 */
class AuthorsSpec extends FlatSpec with Matchers {

  "Authors" should "load name, @twitter and emmail@address" in {
    val file = Paths.get("model/src/test/resources/authors-spec").toUri.toURL
    val resources = Resources.Classpath(new URLClassLoader(Array(file), ClassLoader.getSystemClassLoader))
    Authors(resources).map { authors =>
      authors("foo") shouldBe None
      authors("foo1") shouldBe Some(Author(name"foo1"))
      authors("foo2") shouldBe Some(Author(name"foo2", Some("bar")))
      authors("foo3") shouldBe Some(Author(name"foo3", None, Some("bar@example.com")))
      authors("foo4") shouldBe Some(Author(name"foo4", Some("bar"), Some("baz@example.com")))
      authors("foo5") shouldBe None
    }.unsafeRunSync()
  }

}
