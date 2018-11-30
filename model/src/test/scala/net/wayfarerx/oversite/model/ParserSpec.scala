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
          Vector(Markup.Text("A valid document."))
        ),
        Vector(Markup.Paragraph(Vector(
          Markup.Text("The document content with a "),
          Markup.Link.Load(Pointer.External(Pointer.Page, "https://wayfarerx.net/"), None, Vector(Markup.Text("link"))),
          Markup.Text(".")
        ))),
        Vector(
          Document.Section(
            Vector(Markup.Text("Inlines")),
            Vector(
              Markup.Paragraph(Vector(Markup.Code(Vector(Markup.Text("code"))))),
              Markup.Paragraph(Vector(Markup.Code(Vector(Markup.Text("codes"))))),
              Markup.Paragraph(Vector(Markup.Emphasized(Vector(Markup.Text("emphasis"))))),
              Markup.Paragraph(Vector(Markup.Strong(Vector(Markup.Text("strong"))))),
              Markup.Paragraph(Vector(Markup.Image(Pointer.Image("img.gif"), None, None))),
              Markup.Paragraph(Vector(Markup.Image(Pointer.Image("img.gif"), None, Some("alt")))),
              Markup.Paragraph(Vector(Markup.Image(Pointer.Image("img.gif"), Some("Img"), Some("alt")))),
              Markup.Paragraph(Vector(Markup.Link.Jump(
                "id", None, Vector(Markup.Text("jump"))))),
              Markup.Paragraph(Vector(Markup.Link.Load(
                Pointer.Page(Location.empty, "foo.html"), Some("Foo"), Vector(Markup.Text("load"))))),
              Markup.Paragraph(Vector(Markup.Link.LoadAndJump(
                Pointer.Page(Path.empty, "foo.html"), "id", None, Vector(Markup.Text("loadAndJump")))))
            ),
            Vector.empty
          ),
          Document.Section(
            Vector(Markup.Text("Blocks")),
            Vector(
              Markup.CodeBlock(Vector(Markup.Text("code block"))),
              Markup.BlockQuote(Vector(Markup.Paragraph(Vector(Markup.Text("block quote"))))),
              Markup.HorizontalRule,
              Markup.List.Ordered(Vector(
                Markup.List.Item(Vector(Markup.Paragraph(Vector(Markup.Text("Ordered"))))),
                Markup.List.Item(Vector(Markup.Paragraph(Vector(Markup.Text("List")))))
              )),
              Markup.List.Unordered(Vector(
                Markup.List.Item(Vector(Markup.Paragraph(Vector(Markup.Text("Unordered"))))),
                Markup.List.Item(Vector(Markup.Paragraph(Vector(Markup.Text("List")))))
              ))
            ),
            Vector(Document.Section(
              Vector(Markup.Text("Subsection")),
              Vector(Markup.Paragraph(Vector(Markup.Text("subsection content")))),
              Vector.empty
            ))
          ),
          Document.Section(
            Vector(Markup.Text("End")),
            Vector.empty,
            Vector.empty
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
          Vector(Markup.Text("No author at all."))
        ),
        Vector.empty,
        Vector.empty
      )
    Parser.parse(getClass.getResource("/parser-spec/no-author-again.md")).unsafeRunSync() shouldBe
      Document(
        Metadata(
          name"no-author-again",
          None,
          Vector(
            Markup.Text("No author again. "),
            Markup.Link.Load(Pointer.External(Pointer.Page, "https://wayfarerx.net/"), None, Vector(Markup.Text("x")))
          )
        ),
        Vector.empty,
        Vector.empty
      )
    a[Parser.Problem] should be thrownBy
      Parser.parse(getClass.getResource("/parser-spec/no-description.md")).unsafeRunSync()
  }

}
