/*
 * Website.scala
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

/**
 * The full description of a website.
 *
 * @param environment The environment this website operates in.
 * @param site        The description of this website.
 * @param owner       The author that owns this site.
 * @param root        The root of this website.
 */
case class Website private(
  environment: Environment,
  site: Site,
  owner: Author,
  root: Page.Root[_ <: AnyRef]
)

/**
 * Factory for websites.
 */
object Website {

  /**
   * Attempts to create a new website.
   *
   * @tparam T The type of source that defines the site.
   * @param source      The source of the object that defines the site.
   * @param environment The environment to operate in.
   * @return The result of attempting to create a new website.
   */
  def apply[T: Source](source: T)(implicit environment: Environment): IO[Website] = for {
    site <- implicitly[Source[T]].toSite(source)
    authors <- Authors(environment, "authors.txt")
    owner <- authors(site.owner) map IO.pure getOrElse
      IO.raiseError(new IllegalArgumentException(s"Owner ${site.owner} not found"))
    resource <- environment.find("index.md")
    website <- resource.map { r =>
      IO.pure(Website(environment, site, owner, Page.Root(site, environment, authors, site.assetTypes, site.scopes, r)))
    } getOrElse IO.raiseError(new IllegalArgumentException("Root index.md not found"))
  } yield website

  /**
   * Base type for the source of a site.
   *
   * @tparam T The supported type.
   */
  trait Source[T] {

    /**
     * Creates a site from a source.
     *
     * @param source      The source to create a site from.
     * @param environment The environment to operate in.
     * @return A site created from the source.
     */
    def toSite(source: T)(implicit environment: Environment): IO[Site]

  }

  /**
   * Definitions of the supported site source types.
   */
  object Source {

    /** The source for existing sites. */
    implicit final val ForSite: Source[Site] = new Source[Site] {
      override def toSite(source: Site)(implicit environment: Environment): IO[Site] =
        IO.pure(source)
    }

    /** The source for new instances of site classes. */
    implicit final val ForClass: Source[Class[_]] = new Source[Class[_]] {
      override def toSite(source: Class[_])(implicit environment: Environment): IO[Site] =
        IO(source.newInstance.asInstanceOf[Site]) flatMap ForSite.toSite
    }

    /** The source for new instances of site classes by name. */
    implicit final val ForName: Source[String] = new Source[String] {
      override def toSite(source: String)(implicit environment: Environment): IO[Site] =
        IO(environment.classLoader.loadClass(source)) flatMap ForClass.toSite
    }

  }

}