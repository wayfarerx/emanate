/*
 * Styles.scala
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

import java.net.URI

/**
 * Base class for references to stylesheets.
 */
sealed trait Styles

/**
 * Definitions of the supported stylesheet references.
 */
object Styles {

  /**
   * A stylesheet that can be generated on demand.
   *
   * @param href The virtual location of the stylesheet in the site.
   * @param generate The function that generates the stylesheet.
   */
  case class Generated(href: String, generate: () => String) extends Styles

  /**
   * A stylesheet that is stored with the site.
   *
   * @param asset The asset that identifies the internal stylesheet.
   */
  case class Internal(asset: Asset[Asset.Stylesheet]) extends Styles

  /**
   * A stylesheet that is stored outside the site.
   *
   * @param uri The URI that identifies the external stylesheet.
   */
  case class External(uri: URI) extends Styles

}
