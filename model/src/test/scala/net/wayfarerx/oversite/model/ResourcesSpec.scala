/*
 * ResourcesSpec.scala
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

package net.wayfarerx.oversite.model

import java.io.{BufferedReader, InputStreamReader}
import java.net.{URL, URLClassLoader}
import java.nio.file.Paths

import collection.JavaConverters._

import cats.effect.IO
import org.scalatest.{FlatSpec, Matchers}

/**
 * Test suite for the resource manager.
 */
class ResourcesSpec extends FlatSpec with Matchers {

  "Resources" should "find and list files from directories" in {
    val file = Paths.get("model/src/test/resources/resources-spec/file-element").toUri.toURL
    val resources = Resources.Classpath(new URLClassLoader(Array(file), ClassLoader.getSystemClassLoader))
    resources.find("oversite").unsafeRunSync().map(_.toExternalForm) shouldBe
      Some(file.toExternalForm + "oversite/")
    resources.find("oversite/both/").unsafeRunSync().map(_.toExternalForm) shouldBe
      Some(file.toExternalForm + "oversite/both/")
    resources.find("oversite/both/both.txt").unsafeRunSync().map(_.toExternalForm) shouldBe
      Some(file.toExternalForm + "oversite/both/both.txt")
    resources.find("oversite/file").unsafeRunSync().map(_.toExternalForm) shouldBe
      Some(file.toExternalForm + "oversite/file/")
    resources.find("oversite/file/item.txt").unsafeRunSync().map(_.toExternalForm) shouldBe
      Some(file.toExternalForm + "oversite/file/item.txt")
    resources.find("oversite/both/both.txt").flatMap(_ map readUrl getOrElse IO.pure(None))
      .unsafeRunSync() shouldBe Some("file-both")
    resources.find("oversite/file/item.txt").flatMap(_ map readUrl getOrElse IO.pure(None))
      .unsafeRunSync() shouldBe Some("file-item")
    resources.find("_oversite/").unsafeRunSync() shouldBe None
    resources.find("oversite/_file/").unsafeRunSync().map(_.toExternalForm) shouldBe None
    resources.find("oversite/file/_item.txt").unsafeRunSync().map(_.toExternalForm) shouldBe None
    resources.list("oversite").unsafeRunSync().sorted shouldBe
      Vector("oversite/both/", "oversite/file/")
    resources.list("oversite/both/").unsafeRunSync().sorted shouldBe
      Vector("oversite/both/both.txt")
    resources.list("oversite/file/").unsafeRunSync().sorted shouldBe
      Vector("oversite/file/item.txt")
    resources.list("oversite/file/item.txt").unsafeRunSync().sorted shouldBe Vector.empty
  }

  it should "find and list files from jars" in {
    val jar = Paths.get("model/src/test/resources/resources-spec/jar-element.jar").toUri.toURL
    val resources = Resources.Classpath(new URLClassLoader(Array(jar), ClassLoader.getSystemClassLoader))
    resources.find("oversite").unsafeRunSync().map(_.toExternalForm) shouldBe
      Some(s"jar:${jar.toExternalForm}!/oversite/")
    resources.find("oversite/both").unsafeRunSync().map(_.toExternalForm) shouldBe
      Some(s"jar:${jar.toExternalForm}!/oversite/both/")
    resources.find("oversite/both/both.txt").unsafeRunSync().map(_.toExternalForm) shouldBe
      Some(s"jar:${jar.toExternalForm}!/oversite/both/both.txt")
    resources.find("oversite/jar/").unsafeRunSync().map(_.toExternalForm) shouldBe
      Some(s"jar:${jar.toExternalForm}!/oversite/jar/")
    resources.find("oversite/jar/item.txt").unsafeRunSync().map(_.toExternalForm) shouldBe
      Some(s"jar:${jar.toExternalForm}!/oversite/jar/item.txt")
    resources.find("oversite/both/both.txt").flatMap(_ map readUrl getOrElse IO.pure(None))
      .unsafeRunSync() shouldBe Some("jar-both")
    resources.find("oversite/jar/item.txt").flatMap(_ map readUrl getOrElse IO.pure(None))
      .unsafeRunSync() shouldBe Some("jar-item")
    resources.find("_oversite/").unsafeRunSync() shouldBe None
    resources.find("oversite/_jar/").unsafeRunSync().map(_.toExternalForm) shouldBe None
    resources.find("oversite/jar/_item.txt").unsafeRunSync().map(_.toExternalForm) shouldBe None
    resources.list("oversite").unsafeRunSync().sorted shouldBe
      Vector("oversite/both/", "oversite/jar/")
    resources.list("oversite/both/").unsafeRunSync().sorted shouldBe
      Vector("oversite/both/both.txt")
    resources.list("oversite/jar/").unsafeRunSync().sorted shouldBe
      Vector("oversite/jar/item.txt")
    resources.list("oversite/jar/item.txt").unsafeRunSync().sorted shouldBe Vector.empty
  }

  it should "find and list files from generic class loaders" in {
    val file = Paths.get("model/src/test/resources/resources-spec/file-element").toUri.toURL
    val resources = Resources.Classpath(
      new ClassLoader(new URLClassLoader(Array(file), ClassLoader.getSystemClassLoader)) {})
    resources.find("oversite").unsafeRunSync().map(_.toExternalForm) shouldBe
      Some(file.toExternalForm + "oversite")
    resources.find("oversite/both/").unsafeRunSync().map(_.toExternalForm) shouldBe
      Some(file.toExternalForm + "oversite/both/")
    resources.find("oversite/both/both.txt").unsafeRunSync().map(_.toExternalForm) shouldBe
      Some(file.toExternalForm + "oversite/both/both.txt")
    resources.find("oversite/file").unsafeRunSync().map(_.toExternalForm) shouldBe
      Some(file.toExternalForm + "oversite/file")
    resources.find("oversite/file/item.txt").unsafeRunSync().map(_.toExternalForm) shouldBe
      Some(file.toExternalForm + "oversite/file/item.txt")
    resources.find("oversite/both/both.txt").flatMap(_ map readUrl getOrElse IO.pure(None))
      .unsafeRunSync() shouldBe Some("file-both")
    resources.find("oversite/file/item.txt").flatMap(_ map readUrl getOrElse IO.pure(None))
      .unsafeRunSync() shouldBe Some("file-item")
    resources.find("_oversite/").unsafeRunSync() shouldBe None
    resources.find("oversite/_file/").unsafeRunSync().map(_.toExternalForm) shouldBe None
    resources.find("oversite/file/_item.txt").unsafeRunSync().map(_.toExternalForm) shouldBe None
    resources.list("oversite").unsafeRunSync().sorted shouldBe
      Vector("oversite/both", "oversite/file")
    resources.list("oversite/both/").unsafeRunSync().sorted shouldBe
      Vector("oversite/both/both.txt")
    resources.list("oversite/file/").unsafeRunSync().sorted shouldBe
      Vector("oversite/file/item.txt")
    resources.list("oversite/file/missing").unsafeRunSync().sorted shouldBe Vector.empty
  }

  it should "ignore unsupported URL protocols" in {
    val resources = Resources.Classpath(new URLClassLoader(
      Array(new URL("http://example.com/oversite.jar")),
      ClassLoader.getSystemClassLoader)
    )
    resources.find("oversite").unsafeRunSync() shouldBe None
  }

  // Read the text content of a URL.
  private def readUrl(url: URL): IO[Option[String]] =
    IO(url.openStream()).bracket { stream =>
      for {
        reader <- IO(new InputStreamReader(stream))
        buffered <- IO(new BufferedReader(reader))
        content <- IO(buffered.lines.iterator.asScala.mkString("\n").trim)
      } yield if (content.isEmpty) None else Some(content)
    }(s => IO(s.close()))

}
