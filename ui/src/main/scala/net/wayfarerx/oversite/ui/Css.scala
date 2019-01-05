/*
 * Css.scala
 *
 * Copyright 2018-2019 wayfarerx <x@wayfarerx.net> (@thewayfarerx)
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
package ui

import cats.effect.IO

/**
 * Base type for custom CSS files.
 */
trait Css extends scalacss.StyleSheet.Inline

/**
 * Factory for CSS generators.
 */
object Css {

  /**
   * Creates a named generator for a CSS file.
   *
   * @param name The name of the generated CSS file.
   * @param css  The content of the generated CSS file.
   * @return A named generator for the CSS file.
   */
  def apply(name: Name, css: Css): Scope.Generator = create(name, _ => css)

  /**
   * Creates a named generator for a contextual CSS file.
   *
   * @param name The name of the generated CSS file.
   * @param css  The contextual content of the generated CSS file.
   * @return A named generator for the contextual CSS file.
   */
  def apply(name: Name)(css: Context => Css): Scope.Generator = create(name, css)

  /**
   * Creates a named generator for a contextual CSS file.
   *
   * @param name The name of the generated CSS file.
   * @param css  The contextual content of the generated CSS file.
   * @return A named generator for the contextual CSS file.
   */
  final private def create(name: Name, css: Context => Css): Scope.Generator = {
    val settings = scalacss.devOrProdDefaults
    import settings._
    Scope.Generator(name, Pointer.Stylesheet.css, ctx => IO(css(ctx).render[String] getBytes "UTF-8"))
  }

}
