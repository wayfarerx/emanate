/*
 * ParserSpec.scala
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

import cats.effect.IO

import org.scalatest._

/**
 * Test case for the markdown parser.
 */
class ParserSpec extends FlatSpec with Matchers with EnvironmentSupport {

  behavior of "Parser"

  it should "parse a document" in {
    import Markup._

    val doc = IO(this.getClass.getResourceAsStream("parser-test.md")).bracket {
      new Parser(environment, Asset.Types.default) parse _
    }(stream => IO(stream.close())).unsafeRunSync()
    doc shouldBe Document(
      Name("Test Page").get,
      Vector(Text("This "), Strong(Vector(Text("describes"))), Text(" the test page. ")),
      Some("thewayfarerx"),
      Vector(Paragraph(Vector(Text("This is the main "), Emphasized(Vector(Text("content"))), Text(".")))),
      Vector(
        Section(2, Vector(Text("Section 1")), Vector(
          List.Unordered(Vector(
            List.Item(Vector(Paragraph(Vector(Text("an"))))),
            List.Item(Vector(Paragraph(Vector(Text("unordered"))))),
            List.Item(Vector(Paragraph(Vector(Text("list")))))
          )),
          BlockQuote(Vector(Paragraph(Vector(Text("and a blockquote")))))
        ), Vector()),
        Section(2, Vector(Text("Section 2")), Vector(
          List.Ordered(Vector(
            List.Item(Vector(Paragraph(Vector(Text("an"))))),
            List.Item(Vector(Paragraph(Vector(Text("ordered"))))),
            List.Item(Vector(Paragraph(Vector(Text("list")))))
          )),
          Paragraph(Vector(Code(Vector(Text("and some code")))))
        ), Vector(
          Section(3, Vector(Text("Section 2.1")), Vector(
            Paragraph(Vector(Link.External("https://google.com", None, Vector(Text("A Link"))))),
            Paragraph(Vector(Image(Asset.Image("image").get, None, Some("An image"))))
          ), Vector())
        ))
      )
    )

  }

}
