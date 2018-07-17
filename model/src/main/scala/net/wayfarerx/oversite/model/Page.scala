/*
 * Page.scala
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

import java.nio.file.{Path => JPath}

import ref.SoftReference

import cats.effect.IO

/**
 * Base class for a page in the tree of pages that make up a site.
 *
 * @tparam T The type of entity this page represents.
 */
sealed trait Page[T <: AnyRef] extends Context {

  import Page._

  /** The cached entity value. */
  @volatile
  private var cachedDocument: Option[SoftReference[Markup.Document]] = None

  /** The cached entity value. */
  @volatile
  private var cachedEntity: Option[SoftReference[T]] = None

  /** The environment that contains this page. */
  def environment: Environment

  /** The scope defined for this page. */
  def scope: Scope[T]

  /** The name of this page if it has one. */
  def name: Option[Name]

  /** The document that describes this page. */
  def document: JPath

  /** The location of this page in the site. */
  def location: Location

  /** The parent page if one exists. */
  def parent: Option[Page.Parent[_ <: AnyRef]]

  /** The nested children of this page. */
  def children: IO[Vector[Page.Child[_ <: AnyRef]]]

  /** The title of this page. */
  final lazy val title: IO[Name] =
    load() map (_.name)

  /** The description of this page. */
  final lazy val description: IO[Vector[Markup.Inline]] =
    load() map (_.description)

  /**
   * Attempts to load this page's document.
   *
   * @return The loaded document if available.
   */
  final def load(): IO[Markup.Document] =
    cachedDocument flatMap (_.get) map IO.pure getOrElse {
      new Parser(environment) parse document map { doc =>
        cachedDocument = Some(SoftReference(doc))
        doc
      }
    }

  /**
   * Attempts to decode this page's entity.
   *
   * @return The decoded entity if available.
   */
  final def decode(): IO[T] =
    cachedEntity flatMap (_.get) map IO.pure getOrElse {
      load() flatMap (scope.decoder.decode(_)(this)) map { entity =>
        cachedEntity = Some(SoftReference(entity))
        entity
      }
    }

  /**
   * Attempts to publish this page's entity.
   *
   * @return The published entity if available.
   */
  final def publish(): IO[String] =
    decode() flatMap (scope.publisher.publish(_)(this))

  override def resolve[U <: Asset.Type](asset: Asset[U]): IO[Option[Asset.Resolved[U]]] = ???

  override def resolve[U <: AnyRef](entity: Entity[U]): IO[Option[Entity.Resolved[U]]] = ???

  override def load[U <: AnyRef](entity: Entity[U]): IO[Option[U]] = ???

}

/**
 * Definitions of the supported page types.
 */
object Page {

  /**
   * Base type for pages that can have children.
   *
   * @tparam T The type of entity this page represents.
   */
  sealed trait Parent[T <: AnyRef] extends Page[T] {

    /* Collect the children. */
    final override lazy val children: IO[Vector[Child[_ <: AnyRef]]] = ???

  }

  /**
   * Base type for pages that have parents.
   *
   * @tparam T The type of entity this page represents.
   */
  sealed trait Child[T <: AnyRef] extends Page[T] {

    /** The parent of this page. */
    def parentPage: Parent[_ <: AnyRef]

    /** The name of this child. */
    def childName: Name

    /* Use the parent's environment. */
    final override def environment: Environment = parentPage.environment

    /* Use the child name. */
    final override lazy val name: Option[Name] = Some(childName)

    /* Resolve against the parent's location. */
    final override lazy val location: Location = Location(parentPage.location.path :+ childName).get

    /* Use the parent page. */
    final override lazy val parent: Option[Parent[_ <: AnyRef]] = Some(parentPage)

  }

  /**
   * The root node of a site.
   *
   * @tparam T The type of entity this page represents.
   * @param environment The environment to operate in.
   * @param scope The root scope of the site.
   * @param document The document that describes this page.
   */
  case class Root[T <: AnyRef](
    environment: Environment,
    scope: Scope[T],
    document: JPath
  ) extends Parent[T] {

    /* The root has no name. */
    override def name: Option[Name] = None

    /* The root has no location. */
    override def location: Location = Location.empty

    /* The root has no parent. */
    override def parent: Option[Parent[_ <: AnyRef]] = None

  }

  /**
   * A child node in a site that can contain children.
   *
   * @tparam T The type of entity this page represents.
   * @param childName The name of this child.
   * @param scope The scope of this branch.
   * @param document The document that describes this page.
   * @param parentPage The parent of this page.
   */
  case class Branch[T <: AnyRef](
    childName: Name,
    scope: Scope[T],
    document: JPath,
    parentPage: Parent[_ <: AnyRef]
  ) extends Parent[T] with Child[T]


  /**
   * A child node in a site that cannot contain children.
   *
   * @tparam T The type of entity this page represents.
   * @param childName The name of this child.
   * @param scope The scope of this branch.
   * @param document The document that describes this page.
   * @param parentPage The parent of this page.
   */
  case class Leaf[T <: AnyRef](
    childName: Name,
    scope: Scope[T],
    document: JPath,
    parentPage: Parent[_ <: AnyRef]
  ) extends Child[T] {

    /* Leaves have no children. */
    override val children: IO[Vector[Child[_ <: AnyRef]]] = IO.pure(Vector.empty)

  }

}
