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

import org.scalatest.{FlatSpec, Matchers}

/**
 * Test suite for the parser implementation.
 */
class ParserSpec extends FlatSpec with Matchers {

  "Parser" should "parse valid markdown documents" in {
    Parser.parse(getClass.getResource("/parser-spec/valid.md")).unsafeRunSync() shouldBe
      Document(
        Metadata(
          name"valid-document",
          Some(Author(name"wayfarerx")),
          List(Markup.Text("A valid document."))
        ),
        List(Markup.Paragraph(List(
          Markup.Text("The document content with a "),
          Markup.Link.Load(Pointer.External(Pointer.Page, "https://wayfarerx.net/"), None, List(Markup.Text("link"))),
          Markup.Text(".")
        ))),
        List(
          Document.Section(
            List(Markup.Text("Inlines")),
            List(
              Markup.Paragraph(List(Markup.Code(List(Markup.Text("code"))))),
              Markup.Paragraph(List(Markup.Code(List(Markup.Text("codes"))))),
              Markup.Paragraph(List(Markup.Emphasized(List(Markup.Text("emphasis"))))),
              Markup.Paragraph(List(Markup.Strong(List(Markup.Text("strong"))))),
              Markup.Paragraph(List(Markup.Image(Pointer.Image("img.gif"), None, None))),
              Markup.Paragraph(List(Markup.Image(Pointer.Image("img.gif"), None, Some("alt")))),
              Markup.Paragraph(List(Markup.Image(Pointer.Image("img.gif"), Some("Img"), Some("alt")))),
              Markup.Paragraph(List(Markup.Link.Jump(
                "id", None, List(Markup.Text("jump"))))),
              Markup.Paragraph(List(Markup.Link.Load(
                Pointer.Page(Location.empty, "foo.html"), Some("Foo"), List(Markup.Text("load"))))),
              Markup.Paragraph(List(Markup.Link.LoadAndJump(
                Pointer.Page(Path.empty, "foo.html"), "id", None, List(Markup.Text("loadAndJump")))))
            ),
            List.empty
          ),
          Document.Section(
            List(Markup.Text("Blocks")),
            List(
              Markup.CodeBlock(List(Markup.Text("code block"))),
              Markup.BlockQuote(List(Markup.Paragraph(List(Markup.Text("block quote"))))),
              Markup.HorizontalRule,
              Markup.Listing.Ordered(List(
                Markup.Listing.Item(List(Markup.Paragraph(List(Markup.Text("Ordered"))))),
                Markup.Listing.Item(List(Markup.Paragraph(List(Markup.Text("List")))))
              )),
              Markup.Listing.Unordered(List(
                Markup.Listing.Item(List(Markup.Paragraph(List(Markup.Text("Unordered"))))),
                Markup.Listing.Item(List(Markup.Paragraph(List(Markup.Text("List")))))
              ))
            ),
            List(Document.Section(
              List(Markup.Text("Subsection")),
              List(Markup.Paragraph(List(Markup.Text("subsection content")))),
              List.empty
            ))
          ),
          Document.Section(
            List(Markup.Text("End")),
            List.empty,
            List.empty
          )
        )
      )
  }

  it should "handle common syntax variations" in {
    a[Parser.Problem] should be thrownBy
      Parser.parse(getClass.getResource("/parser-spec/invalid-name.md")).unsafeRunSync()
    Parser.parse(getClass.getResource("/parser-spec/no-author.md")).unsafeRunSync() shouldBe
      Document(
        Metadata(
          name"no-author",
          None,
          List(Markup.Text("No author at all."))
        ),
        List.empty,
        List.empty
      )
    Parser.parse(getClass.getResource("/parser-spec/no-author-again.md")).unsafeRunSync() shouldBe
      Document(
        Metadata(
          name"no-author-again",
          None,
          List(
            Markup.Text("No author again. "),
            Markup.Link.Load(Pointer.External(Pointer.Page, "https://wayfarerx.net/"), None, List(Markup.Text("x")))
          )
        ),
        List.empty,
        List.empty
      )
    a[Parser.Problem] should be thrownBy
      Parser.parse(getClass.getResource("/parser-spec/no-description.md")).unsafeRunSync()
  }

}
