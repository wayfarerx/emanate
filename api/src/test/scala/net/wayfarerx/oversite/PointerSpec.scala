/*
 * PointerSpec.scala
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
 * Test suite for the pointer implementation.
 */
class PointerSpec extends FlatSpec with Matchers {

  import Pointer.{EntityExtensions => _, _}

  "Pointer" should "handle prefixes correctly" in {
    Pointer.Prefix.Relative(Path("r")).toLocation(Location.empty) shouldBe
      Some(Location.resolved(Path("r")))
    Pointer.Prefix.Relative(Path("r")).toLocation(Location.resolved(Path("l"))) shouldBe
      Some(Location.resolved(Path("l/r")))
    Pointer.Prefix.Absolute(Location.resolved(Path("a"))).toLocation(Location.empty) shouldBe
      Some(Location.resolved(Path("a")))
    Pointer.Prefix.Absolute(Location.resolved(Path("a"))).toLocation(Location.resolved(Path("l"))) shouldBe
      Some(Location.resolved(Path("a")))
  }

  it should "handle entity pointers correctly" in {
    val entity = Entity[String]
    val name = name"name"
    val relative = Prefix.Relative(Path("relative"))
    val absolute = Prefix.Absolute(Location.resolved(Path("absolute")))
    // Verify entity pointer type equality.
    entity == Entity[String] shouldBe true
    (entity: Entity[_]) == Entity[Vector[String]] shouldBe false
    // Verify entity pointer construction.
    entity(name) shouldBe Search[Entity[String]](entity, Prefix.empty, name)
    entity(relative.path, name) shouldBe Search[Entity[String]](entity, relative, name)
    entity(absolute.location, name) shouldBe Search[Entity[String]](entity, absolute, name)
    entity(relative.path) shouldBe Target[Entity[String], Unit](entity, relative, ())
    entity(absolute.location) shouldBe Target[Entity[String], Unit](entity, absolute, ())
    // Verify entity pointer parsing.
    entity.parse("name") shouldBe Search(entity, Prefix.empty, name)
    entity.parse(relative.toString + "name") shouldBe Search(entity, relative, name)
    entity.parse(absolute.toString + "name") shouldBe Search(entity, absolute, name)
    entity.parse(relative.toString) shouldBe Target[Entity[String], Unit](entity, relative, ())
    entity.parse(absolute.toString) shouldBe Target[Entity[String], Unit](entity, absolute, ())
    // Verify entity href construction.
    entity.href(Prefix.empty, ()) shouldBe "./"
    entity.href(Prefix.root, ()) shouldBe "/"
    entity.href(relative, ()) shouldBe "relative/"
    entity.href(absolute, ()) shouldBe "/absolute/"
    Target[Entity[String], Unit](entity, relative, ()).href shouldBe "relative/"
    // Verify that entity pointers can be narrowed.
    val wideSearchPointer: Pointer[Entity[AnyRef]] = Entity[AnyRef].parse("name")
    val wideTargetPointer: Pointer[Entity[AnyRef]] = Entity[AnyRef].parse("name/")
    val wideSearchInternal: Internal[Entity[AnyRef]] = Entity[AnyRef].parse("name")
    val wideTargetInternal: Internal[Entity[AnyRef]] = Entity[AnyRef].parse("name/")
    val wideSearch: Search[Entity[AnyRef]] = Entity[AnyRef].apply(name"name")
    val wideTarget: Target[Entity[AnyRef], Unit] = Entity[AnyRef].apply(Prefix.Relative(Path("name")))
    val narrowSearchPointer: Pointer[Entity[String]] = wideSearchPointer.narrow[String]
    val narrowTargetPointer: Pointer[Entity[String]] = wideTargetPointer.narrow[String]
    val narrowSearchInternal: Internal[Entity[String]] = wideSearchInternal.narrow[String]
    val narrowTargetInternal: Internal[Entity[String]] = wideTargetInternal.narrow[String]
    val narrowSearch: Search[Entity[String]] = wideSearch.narrow[String]
    val narrowTarget: Target[Entity[String], Unit] = wideTarget.narrow[String]
    narrowSearchPointer shouldBe entity.parse("name")
    narrowTargetPointer shouldBe entity.parse("name/")
    narrowSearchInternal shouldBe entity.parse("name")
    narrowTargetInternal shouldBe entity.parse("name/")
    narrowSearch shouldBe entity.parse("name")
    narrowTarget shouldBe entity.parse("name/")
  }

  it should "handle asset pointers correctly" in {
    Seq(Page, Image, Stylesheet, Script) flatMap (a => a.variants.toSeq map (a -> _)) foreach {
      case (asset, extension) =>
        val name = name"name"
        val relative = Prefix.Relative(Path("relative"))
        val absolute = Prefix.Absolute(Location.resolved(Path("absolute")))
        val suffix = asset.name + "." + extension
        // Verify asset pointer construction.
        asset(name) shouldBe Search[Asset](asset, Prefix.empty, name)
        asset(relative.path, name) shouldBe Search[Asset](asset, relative, name)
        asset(absolute.location, name) shouldBe Search[Asset](asset, absolute, name)
        asset(suffix) shouldBe Target[Asset, String](asset, Prefix.empty, suffix)
        asset(relative.path, suffix) shouldBe Target[Asset, String](asset, relative, suffix)
        asset(absolute.location, suffix) shouldBe Target[Asset, String](asset, absolute, suffix)
        // Verify asset pointer parsing.
        asset.parse("name") shouldBe Search(asset, Prefix.empty, name)
        asset.parse(relative.toString + "name") shouldBe Search(asset, relative, name)
        asset.parse(absolute.toString + "name") shouldBe Search(asset, absolute, name)
        asset.parse(suffix) shouldBe Target[Asset, String](asset, Prefix.empty, suffix)
        asset.parse(relative.toString + suffix) shouldBe Target[Asset, String](asset, relative, suffix)
        asset.parse(absolute.toString + suffix) shouldBe Target[Asset, String](asset, absolute, suffix)
        asset.parse(s"//example.com/$suffix") shouldBe External(asset, s"//example.com/$suffix")
        // Verify asset href construction.
        asset.href(Prefix.empty, suffix) shouldBe suffix
        asset.href(Prefix.root, suffix) shouldBe s"/$suffix"
        asset.href(relative, suffix) shouldBe s"relative/$suffix"
        asset.href(absolute, suffix) shouldBe s"/absolute/$suffix"
    }
  }

  it should "parse untyped pointers" in {
    val name = name"name"
    val path = Path("at")
    val location = Location.resolved(path)
    Pointer.parse("") shouldBe Target(Entity[AnyRef], Prefix.empty, ())
    Pointer.parse(".") shouldBe Target(Entity[AnyRef], Prefix.current, ())
    Pointer.parse("/") shouldBe Target(Entity[AnyRef], Prefix.root, ())
    Pointer.parse("name") shouldBe Search(Entity[AnyRef], Prefix.empty, name)
    Pointer.parse("/name") shouldBe Search(Entity[AnyRef], Prefix.root, name)
    Pointer.parse("at/name") shouldBe Search(Entity[AnyRef], Prefix.Relative(path), name)
    Pointer.parse("/at/name") shouldBe Search(Entity[AnyRef], Prefix.Absolute(location), name)
    Pointer.parse("name/") shouldBe Target(Entity[AnyRef], Prefix.Relative(Path(name)), ())
    Pointer.parse("/name/") shouldBe Target(Entity[AnyRef], Prefix.Absolute(Location.resolved(Path(name))), ())
    Pointer.parse("at/name/") shouldBe Target(Entity[AnyRef], Prefix.Relative(path :+ name), ())
    Pointer.parse("/at/name/") shouldBe Target(Entity[AnyRef], Prefix.Absolute(location :+ name), ())
    Seq(Page, Image, Stylesheet, Script) flatMap (a => a.variants.toSeq map (a -> _)) foreach {
      case (asset, variant) =>
        asset.prefix foreach { p =>
          Pointer.parse(s"$p/") shouldBe Search(asset, Prefix.empty, asset.name)
          Pointer.parse(s"/$p/") shouldBe Search(asset, Prefix.root, asset.name)
        }
        variant.extensions foreach { extension =>
          val suffix = asset.name + "." + extension
          Pointer.parse(suffix) shouldBe Target(asset, Prefix.empty, suffix)
          Pointer.parse(s"/$suffix") shouldBe Target(asset, Prefix.root, suffix)
          Pointer.parse(s"at/$suffix") shouldBe Target(asset, Prefix.Relative(path), suffix)
          Pointer.parse(s"/at/$suffix") shouldBe Target(asset, Prefix.Absolute(location), suffix)
          Pointer.parse(s"//example.com/$suffix") shouldBe External(asset, s"//example.com/$suffix")
        }
    }
    Pointer.parse("//example.com") shouldBe External(Page, "//example.com")
    Pointer.parse("https://example.com/") shouldBe External(Page, "https://example.com/")
    Pointer.parse("mailto:x@wayfarerx.net") shouldBe External(Page, "mailto:x@wayfarerx.net")
  }

  it should "optimize and swap prefixes" in {
    val a = Location.resolved(Path("a"))
    val b = Location.resolved(Path("a/b"))
    val c = Location.resolved(Path("a/b/c"))
    Pointer.Prefix(a, a) shouldBe Pointer.Prefix.Relative(Path.empty)
    Pointer.Prefix(a, b) shouldBe Pointer.Prefix.Relative(Path("b"))
    Pointer.Prefix(a, c) shouldBe Pointer.Prefix.Relative(Path("b/c"))
    Pointer.Prefix(b, a) shouldBe Pointer.Prefix.Relative(Path(".."))
    Pointer.Prefix(b, b) shouldBe Pointer.Prefix.Relative(Path.empty)
    Pointer.Prefix(b, c) shouldBe Pointer.Prefix.Relative(Path("c"))
    Pointer.Prefix(c, a) shouldBe Pointer.Prefix.Absolute(Location.resolved(Path("a")))
    Pointer.Prefix(c, b) shouldBe Pointer.Prefix.Relative(Path(".."))
    Pointer.Prefix(c, c) shouldBe Pointer.Prefix.Relative(Path.empty)
    val p = Pointer.Search(Pointer.Image, Pointer.Prefix.Relative(b.path), name"search")
    val q = Pointer.Target(Pointer.Image, Pointer.Prefix.Absolute(c), "search.png")
    p.withPrefix(Pointer.Prefix.Absolute(a)) shouldBe
      Pointer.Search(Pointer.Image, Pointer.Prefix.Absolute(a), name"search")
    q.withPrefix(Pointer.Prefix.Absolute(a)) shouldBe
      Pointer.Target(Pointer.Image, Pointer.Prefix.Absolute(a), "search.png")
  }

}
