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

import language.implicitConversions

import cats.effect.IO

import scalacss.defaults.Exports
import scalacss.internal.{Compose, Env, Renderer}
import scalacss.internal.mutable.Register.{ErrorHandler, MacroName, NameGen}
import scalacss.internal.mutable.{Register, Settings}

/**
 * Base type for generated CSS files.
 *
 * @param name The name of this generated CSS file.
 * @param settings The `scalacss` settings to use.
 */
abstract class Css(val name: Name, val settings: Settings = scalacss.devOrProdDefaults)
  extends scalacss.StyleSheet.Standalone()(settings.cssRegister) with Exports with Settings {

  /* Return the CSS register. */
  implicit final override val cssRegister: Register = settings.cssRegister

  /** The generator implementation that produces this CSS file's content. */
  final val generator: Scope.Generator =
    Scope.Generator(name, Pointer.Stylesheet.css, generate(_) flatMap (s => IO(s getBytes "UTF-8")))

  /* Return the CSS name generator. */
  final override def cssRegisterNameGen: NameGen = settings.cssRegisterNameGen

  /* Return the CSS name to register as. */
  final override def cssRegisterMacroName: MacroName = settings.cssRegisterMacroName

  /* Return the CSS error handler functions. */
  final override def cssRegisterErrorHandler: ErrorHandler = settings.cssRegisterErrorHandler

  /* Return the CSS string renderer. */
  implicit final override def cssStringRenderer: Renderer[String] = settings.cssStringRenderer

  /* Return the CSS composer. */
  implicit final override def cssComposition: Compose = settings.cssComposition

  /* Return the CSS environment. */
  implicit final override def cssEnv: Env = settings.cssEnv

  /** The generate method that produces this CSS file's content. */
  def generate(context: Context): IO[String] = IO(render[String])

}

/**
 * Factory for CSS generators.
 */
object Css {

  /** Convert all CSS objects into generators. */
  implicit def cssToGenerator(css: Css): Scope.Generator = css.generator

}
