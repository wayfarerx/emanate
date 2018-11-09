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

import cats.effect.IO

/**
 * The collection of all indexed nodes in a site.
 *
 * @param nodesByName     The indexed nodes by name.
 * @param nodesByLocation The indexed nodes by location.
 */
case class Index private(
  nodesByName: Map[Name, Vector[Node[_ <: AnyRef]]],
  nodesByLocation: Map[Location, Node[_ <: AnyRef]]
) {

  /**
   * Searches for a node at the specified location.
   *
   * @param location The location to search for a node at.
   * @return The result of searching for a node at the specified location.
   */
  def apply(location: Location): Option[Node[_ <: AnyRef]] =
    nodesByLocation get location

  /**
   * Searches for a node from the specified location.
   *
   * @param name The name to search for.
   * @param from The location to search from.
   * @return The result of searching for a node from the specified location.
   */
  def apply(name: Name, from: Location): Vector[Node[_ <: AnyRef]] =
    nodesByName get name map (_ sortBy (from distanceTo _.location)) getOrElse Vector.empty

}

/**
 * Factory for site indexes.
 */
object Index {

  /**
   * Creates the index for a node.
   *
   * @param node The node to create the index for.
   * @return The result of creating the index.
   */
  def apply(node: Node[_ <: AnyRef]): IO[Index] = {

    /* Recursively index the site. */
    def index(
      nodes: Vector[Node[_ <: AnyRef]]
    ): IO[(Map[Name, Vector[Node[_ <: AnyRef]]], Map[Location, Node[_ <: AnyRef]])] =
      nodes match {
        case head +: remaining => for {
          identifiers <- head.identifiers
          children <- head match {
            case p: Node.Parent[_] if p.scope.indexed => p.children
            case _ => IO.pure(Vector.empty[Node[_ <: AnyRef]])
          }
          tail <- index(children ++ remaining)
        } yield {
          val (tailByName, tailByLocation) = tail
          (tailByName /: identifiers.map(_ -> Vector(head)).reverse) { (result, pair) =>
            val (key, value) = pair
            result + (key -> (result get key map (value ++ _) getOrElse value))
          } -> (tailByLocation + (head.location -> head))
        }
        case _ =>
          IO.pure(Map.empty[Name, Vector[Node[_ <: AnyRef]]] -> Map.empty[Location, Node[_ <: AnyRef]])
      }

    index(Vector(node)) map { case (n, l) => new Index(n, l) }
  }

}
