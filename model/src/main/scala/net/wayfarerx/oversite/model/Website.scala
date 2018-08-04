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
 * @param root        The root of this website.
 */
case class Website private(
  environment: Environment,
  site: Site,
  root: Page.Root[_ <: AnyRef]
)

/**
 * Factory for websites.
 */
object Website {

  /**
   * Attempts to create a new website.
   *
   * @param className   The name of the class that defines the site.
   * @param environment The environment to operate in.
   * @return The result of attempting to create a new website.
   */
  def apply(className: String)(implicit environment: Environment): IO[Website] = for {
    site <- IO(environment.classLoader.loadClass(className).newInstance.asInstanceOf[Site])
    website <- environment.find("index.md") flatMap {
      case Some(rootDocument) => IO.pure(Website(environment, site,
        Page.Root(environment, site.assetTypes, site.scopes, rootDocument)))
      case None => IO.raiseError(new IllegalArgumentException("Root index.md not found."))
    }
  } yield website

}