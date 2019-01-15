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

import language.existentials
import cats.effect.IO

import scala.reflect.ClassTag

/**
 * The collection of all indexed nodes in a site.
 *
 * @param nodesByName     The indexed nodes by name.
 * @param nodesByLocation The indexed nodes by location.
 * @param nodesByType     The indexed nodes by entity type.
 */
case class Index private(
  nodesByName: Map[Name, List[Node[_ <: AnyRef]]],
  nodesByLocation: Map[Location, Node[_ <: AnyRef]],
  nodesByType: Map[Class[_ <: AnyRef], List[Node[_ <: AnyRef]]]
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
   * Finds a node from the specified location.
   *
   * @param from The location to search from.
   * @param name The name to search for.
   * @return The result of searching for a node from the specified location.
   */
  def apply(from: Location, name: Name): List[Node[_ <: AnyRef]] =
    nodesByName.getOrElse(name, Nil) sortBy (from distanceTo _.location)

  /**
   * Searches for nodes from the specified location.
   *
   * @tparam T The type of entity to search for nodes of.
   * @param from  The location to search from.
   * @param query The query to apply to all nodes assignable to the specified entity type.
   * @return The matching nodes from the specified location.
   */
  def apply[T <: AnyRef : ClassTag](
    from: Location,
    query: Node[_ <: T] => IO[Boolean]
  ): IO[List[Node[_ <: T]]] = {

    def process(remaining: List[Node[_ <: AnyRef]]): IO[List[Node[_ <: T]]] = remaining match {
      case head :: tail => for {
        n <- IO(head.asInstanceOf[Node[_ <: T]])
        h <- query(n)
        t <- process(tail)
      } yield if (h) n +: t else t
      case _ =>
        IO.pure(Nil)
    }

    for {
      cls <- IO(implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[_ <: T]])
      res <- process(nodesByType.getOrElse(cls, Nil))
    } yield res sortBy (from distanceTo _.location)
  }

  /**
   * Combines that index with this index.
   *
   * @param that The index to combine with this index.
   * @return This index combined with that index.
   */
  def ++(that: Index): Index = Index(
    nodesByName ++ that.nodesByName.keys.map { name =>
      name -> (nodesByName.getOrElse(name, Nil) ::: that.nodesByName.getOrElse(name, Nil))
    },
    nodesByLocation ++ that.nodesByLocation,
    nodesByType ++ that.nodesByType.keys.map { tpe =>
      tpe -> (nodesByType.getOrElse(tpe, Nil) ::: that.nodesByType.getOrElse(tpe, Nil))
    }
  )

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
  def apply(node: Node.Root[_ <: AnyRef]): IO[Index] = {

    /* Recursively examine a type hierarchy. */
    def examine(remaining: List[Class[_]]): IO[Set[Class[_ <: AnyRef]]] = remaining match {
      case head :: tail => for {
        self <- IO(Set(head.asInstanceOf[Class[_ <: AnyRef]]))
        parents <- examine(Option(head.getSuperclass).toList ::: head.getInterfaces.toList)
        next <- examine(tail)
      } yield self ++ parents ++ next
      case _ => IO.pure(Set.empty)
    }

    /* Recursively index the site. */
    def index(remaining: List[Node[_ <: AnyRef]]): IO[Index] = remaining match {
      case head :: tail => for {
        identifiers <- head.identifiers
        types <- examine(List(head.scope.classTag.runtimeClass))
        children <- head match {
          case p: Node.Parent[_] if p.scope.indexed => p.children map (_.toList)
          case _ => IO.pure(Nil)
        }
        t <- index(children ::: tail)
      } yield Index(
        identifiers.map(_ -> List(head)).toMap,
        Map(head.location -> head),
        types.map(_ -> List(head)).toMap
      ) ++ t
      case _ =>
        IO.pure(Index(Map.empty, Map.empty, Map.empty))
    }

    index(List(node))
  }

}
