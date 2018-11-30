/*
 * AltTextSpec.scala
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

import org.scalatest.{FlatSpec, Matchers}

/**
 * Test suite for the alt-text cache implementation.
 */
class AltTextSpec extends FlatSpec with Matchers {

  "AltText" should "provide alt-text caches at a location" in {
    val altText = new AltText(Location.resolved(Path("alt-text-spec")), Resources.Classpath(getClass.getClassLoader))
    altText(Path.empty, "foo").unsafeRunSync() shouldBe Some("bar")
    altText(Path.empty, "foo.png").unsafeRunSync() shouldBe Some("bar")
    altText(Path.empty, "foo.jpg").unsafeRunSync() shouldBe Some("bar")
    altText(Path.empty, "bar").unsafeRunSync() shouldBe None
    altText(Path("foo"), "bar").unsafeRunSync() shouldBe None
  }

}
