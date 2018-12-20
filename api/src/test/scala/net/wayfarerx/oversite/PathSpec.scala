/*
 * PathSpec.scala
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
 * Test suite for the path implementation.
 */
class PathSpec extends FlatSpec with Matchers {

  "Path" should "be created from names, strings or regular strings" in {
    Path(name"a", name"b").elements shouldBe Vector(Path.Child(name"a"), Path.Child(name"b"))
    Path("a", "./..").elements shouldBe Vector(Path.Child(name"a"), Path.Current, Path.Parent)
    Path(Path.Regular("a")).elements shouldBe Vector(Path.Child(name"a"))
  }

  it should "parse paths and suffixes from strings" in {
    Path.parse("") shouldBe Path.empty -> None
    Path.parse("suffix") shouldBe Path.empty -> Some("suffix")
    Path.parse("suffix.gif") shouldBe Path.empty -> Some("suffix.gif")
    Path.parse("path/") shouldBe Path("path") -> None
    Path.parse("path/.") shouldBe Path("path/.") -> None
    Path.parse("path/..") shouldBe Path("path/..") -> None
    Path.parse("path/...") shouldBe Path("path") -> Some("...")
    Path.parse("path/./suffix") shouldBe Path("path/.") -> Some("suffix")
    Path.parse("path/../suffix.gif") shouldBe Path("path/..") -> Some("suffix.gif")
  }

  it should "normalize and resolve itself" in {
    val raw = Path("../../path/./to/../somewhere")
    val normalized = raw.normalized
    val resolved = raw.resolved
    normalized shouldBe Path("../../path/somewhere")
    normalized eq normalized.normalized shouldBe true
    resolved shouldBe Path("path/somewhere")
    resolved eq resolved.resolved shouldBe true
  }

  it should "support prepend, append and concatenate" in {
    val path = Path("a")
    name"b" +: path shouldBe Path("b/a")
    "" +: path shouldBe path
    "b/c" +: path shouldBe Path("b/c/a")
    Path.Parent +: path shouldBe Path("../a")
    path :+ "" shouldBe path
    path :+ name"b" shouldBe Path("a/b")
    path :+ "b/c" shouldBe Path("a/b/c")
    path :+ Path.Current shouldBe Path("a/.")
    path ++ Path.empty shouldBe path
    Path.empty ++ path shouldBe path
    path ++ Path("b") shouldBe Path("a/b")
  }

  it should "generate canonical path strings" in {
    Path.empty.toString shouldBe ""
    Path("a").toString shouldBe "a/"
    Path("a/./b/../c").toString shouldBe "a/./b/../c/"
  }

}
