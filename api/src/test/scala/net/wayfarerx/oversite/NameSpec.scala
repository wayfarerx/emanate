/*
 * NameSpec.scala
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
 * Test suite for the name implementation.
 */
class NameSpec extends FlatSpec with Matchers {

  "Name" should "never be empty" in {
    Name("") shouldBe None
    Name(" ") shouldBe None
    Name(".") shouldBe None
    Name("./'") shouldBe None
  }

  it should "Correctly normalize display names" in {
    name"hi".normal shouldBe "hi"
    name" Hi ".normal shouldBe "hi"
    name"Hi there.".normal shouldBe "hi-there"
    name"'Hi there,' she said.".normal shouldBe "hi-there-she-said"
    Name("\"Hi there Mr. Jones,\" she said.").get.normal shouldBe "hi-there-mr-jones-she-said"
    name"`Hi there Mr. Jones,`     she said.".normal shouldBe "hi-there-mr-jones-she-said"
  }

  it should "act as a Product2" in {
    val name = name"Name"
    name._1 shouldBe "name"
    name._2 shouldBe "Name"
    val name2 = name"Name2"
    name.canEqual(name2) shouldBe true
    name.canEqual("name") shouldBe false
    name.equals(name) shouldBe true
    name.equals(name2) shouldBe false
    (name: AnyRef).equals("name") shouldBe false
    name.hashCode == name2.hashCode shouldBe false
    name.toString shouldBe "name"

  }

}
