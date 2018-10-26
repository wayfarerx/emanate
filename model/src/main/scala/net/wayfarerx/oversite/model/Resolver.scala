/*
 * Resolver.scala
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
 * The implementation of a resolvable context.
 */
trait Resolver extends Context {

  /** The type of this context that is resolvable. */
  protected type This >: this.type <: Resolver

  /** The styles provided by this resolver. */
  def styles: Vector[Scope.Styles]

  //
  // The context implementations.
  //

  /* Convert the stylesheets into stylesheet references. */
  override def stylesheets: Vector[Scope.Styles.Reference] = styles map {
    case Scope.Styles.Generated(name, _) => Scope.Styles.Internal(Pointer.Stylesheet(name))
    case reference: Scope.Styles.Reference => reference
  }

  /* Resolve internal and external pointers. */
  final override def resolve[P <: Pointer.Type](pointer: Pointer[P]): IO[Pointer.Resolved[P]] = pointer match {
    case p: Pointer.Internal[P] => resolve(p)
    case p: Pointer.External[P] => IO.pure(p)
  }

  final def resolve[P <: Pointer.Type.Aux[S], S](pointer: Pointer.Internal[P]): IO[Pointer.Target[P, S]] =
    pointer.tpe match {
      case Pointer.Entity(_) => ???
      case asset: Pointer.Asset => ???
    }

  /* Resolve external pointers. */
  final override def resolve[P <: Pointer.Asset](pointer: Pointer.External[P]): IO[Pointer.External[P]] =
    IO.pure(pointer)

  final def load[E <: AnyRef](entity: Pointer.Internal[Pointer.Entity[E]]): IO[E] = for {
    resolved <- resolve(entity)
  } yield {

    ???
  }

  final def alt(image: Pointer.Internal[Pointer.Image]): IO[Option[String]] = {

    ???
  }

  //
  // Abstract find methods.
  //

  protected def findEntity(path: Path): Option[This]

  protected def searchForEntity(name: Name): Option[This]


}
