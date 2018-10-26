/*
 * LocationSpec.scala
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
 * Test suite for the location implementation.
 */
class LocationSpec extends FlatSpec with Matchers {

  "Location" should "be created from resolved paths" in {
    Location(Path("..")) shouldBe None
    Location(Path("a/../..")) shouldBe None
    Location(Path.empty) shouldBe Some(Location.empty)
    Location(Path("a")) shouldBe Some(Location.resolved(Path("a")))
    Location.resolved(Path("../a")) shouldBe Location.resolved(Path("a"))
  }

  it should "expose its parent, composition and children" in {
    Location.empty.parent shouldBe None
    Location.resolved(Path("a")).parent shouldBe Some(Location.empty)
    Location.empty.names shouldBe 'empty
    Location.resolved(Path("a")).names shouldBe Vector(name"a")
    Location.empty :+ name"b" shouldBe Location.resolved(Path("b"))
    Location.resolved(Path("a")) :+ name"b" shouldBe Location.resolved(Path("a/b"))
  }

  it should "calculate common prefixes and distances between locations" in {
    val root = Location.empty
    val a = Location.resolved(Path("/a"))
    val b = Location.resolved(Path("/a/b"))
    val c = Location.resolved(Path("/a/b/c"))
    val d = Location.resolved(Path("/a/b/d"))
    root.commonPrefixWith(a) shouldBe root
    a.commonPrefixWith(root) shouldBe root
    root.commonPrefixWith(b) shouldBe root
    a.commonPrefixWith(a) shouldBe a
    a.commonPrefixWith(b) shouldBe a
    a.commonPrefixWith(c) shouldBe a
    b.commonPrefixWith(c) shouldBe b
    b.commonPrefixWith(d) shouldBe b
    c.commonPrefixWith(d) shouldBe b
    root.distanceTo(root) shouldBe 0
    root.distanceTo(a) shouldBe 1
    a.distanceTo(root) shouldBe 1
    a.distanceTo(b) shouldBe 1
    a.distanceTo(c) shouldBe 2
    a.distanceTo(d) shouldBe 2
  }

  it should "act as a Product1" in {
    val location = Location.resolved(Path("/name"))
    location._1 shouldBe Path("/name")
    val location2 = Location.resolved(Path("/names"))
    location.canEqual(location2) shouldBe true
    location.canEqual(Path("/name")) shouldBe false
    location.equals(location) shouldBe true
    location.equals(location2) shouldBe false
    (location: AnyRef).equals(Path("/name")) shouldBe false
    location.hashCode == location2.hashCode shouldBe false
    location.toString shouldBe "/name/"

  }

}
