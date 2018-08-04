/*
 * Index.scala
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

import reflect.ClassTag

import cats.effect.IO

/**
 * An index of the pages in the site on various keys.
 *
 * @param pagesByName     The pages indexed by name.
 * @param pagesByLocation The pages indexed by location.
 */
case class Index(
  pagesByName: Map[Name, Vector[Page[_ <: AnyRef]]],
  pagesByLocation: Map[Location, Page[_ <: AnyRef]]
) {

  /**
   * Returns the pages with the specified name.
   *
   * @param name The name of the pages to return.
   * @return The pages with the specified name.
   */
  def apply(name: Name): Vector[Page[_ <: AnyRef]] =
    pagesByName.getOrElse(name, Vector.empty)

  /**
   * Returns the page at the specified location if one exists.
   *
   * @param location The location of the desired page.
   * @return The page at the specified location if one exists.
   */
  def apply(location: Location): Option[Page[_ <: AnyRef]] =
    pagesByLocation get location

  /**
   * Returns the pages assignable to the specified type.
   *
   * @param assignableTo The type that pages must be assignable to.
   * @return The pages assignable to the specified type.
   */
  def apply(assignableTo: ClassTag[_]): Vector[Page[_ <: AnyRef]] =
    pagesByLocation.values.toVector filter (assignableTo.runtimeClass isAssignableFrom _.scope.classTag.runtimeClass)

  /**
   * Returns the pages with the specified name and assignable to the specified type.
   *
   * @param name         The name of the pages to return.
   * @param assignableTo The type that pages must be assignable to.
   * @return The pages with the specified name and assignable to the specified type.
   */
  def apply(name: Name, assignableTo: ClassTag[_]): Vector[Page[_ <: AnyRef]] =
    apply(name) filter (assignableTo.runtimeClass isAssignableFrom _.scope.classTag.runtimeClass)

  /**
   * Returns the pages with the specified location and assignable to the specified type.
   *
   * @param location     The location of the desired page.
   * @param assignableTo The type that pages must be assignable to.
   * @return The pages with the specified location and assignable to the specified type.
   */
  def apply(location: Location, assignableTo: ClassTag[_]): Option[Page[_ <: AnyRef]] =
    apply(location) filter (assignableTo.runtimeClass isAssignableFrom _.scope.classTag.runtimeClass)

}

/**
 * Factory for site indicies.
 */
object Index {

  /**
   * Attempts to create an index from a root page.
   *
   * @param root The root page to start searching at.
   * @return The result of the attempt to create a root page.
   */
  def apply(root: Page.Root[_ <: AnyRef]): IO[Index] = {

    /* Recursively search all pages. */
    def search(index: Index, pages: Vector[Page[_ <: AnyRef]]): IO[Index] =
      pages match {
        case head +: tail => for {
          titles <- head.titles
          children <- head.children
          result <- search(index.copy(
            (index.pagesByName /: (head.name ++ titles)) { (pages, id) =>
              pages + (id -> pages.get(id).map(_ :+ head).getOrElse(Vector(head)))
            },
            index.pagesByLocation + (head.location -> head)
          ), children ++ tail)
        } yield result
        case _ => IO.pure(index)
      }

    search(Index(Map.empty, Map.empty), Vector(root))
  }

}