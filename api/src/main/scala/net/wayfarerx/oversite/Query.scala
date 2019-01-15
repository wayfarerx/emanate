/*
 * Query.scala
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

/**
 * Base type for relation queries.
 *
 * @tparam T The type of entity that the relation is from.
 */
sealed trait Query[-T <: AnyRef] {

  import Query._

  /**
   * Returns the inverse of this query.
   *
   * @return The inverse of this query.
   */
  final def unary_! : Query[T] = Not(this)

  /**
   * Returns a query satisfied by this query and `that` query.
   *
   * @tparam U The type of the other query.
   * @param that The other query to satisfy.
   * @return A query satisfied by this query and `that` query.
   */
  final def &[U <: T](that: Query[U]): Query[U] = And(this, that)

  /**
   * Returns a query satisfied by this query or `that` query.
   *
   * @tparam U The type of the other query.
   * @param that The other query to satisfy.
   * @return A query satisfied by this query or `that` query.
   */
  final def |[U <: T](that: Query[U]): Query[U] = Or(this, that)

  /**
   * Returns a query satisfied by this query or `that` query, but not both.
   *
   * @tparam U The type of the other query.
   * @param that The other query to satisfy.
   * @return A query satisfied by this query or `that` query, but not both.
   */
  final def ^[U <: T](that: Query[U]): Query[U] = Xor(this, that)

}

/**
 * Definitions of the supported queries.
 */
object Query {

  /**
   * Creates a query satisfied when any of the specified pointers are present.
   *
   * @tparam T The type of entity that the relation is from.
   * @param relation The relation to query.
   * @param pointers The pointers that satisfy this query.
   * @return A query satisfied when any of the specified pointers are present.
   */
  def apply[T <: AnyRef](
    relation: Relation[T],
    pointers: Pointer[Pointer.Entity[_ <: AnyRef]]*
  ): Query[T] = Related(relation, pointers: _*)

  /**
   * A query satisfied when any of the specified pointers are related to.
   *
   * @tparam T The type of entity that the relation is from.
   * @param relation The relation to query.
   * @param pointers The pointers that satisfy this query.
   */
  case class Related[T <: AnyRef](
    relation: Relation[T],
    pointers: List[Pointer[Pointer.Entity[_ <: AnyRef]]]
  ) extends Query[T]

  /**
   * Factory for any queries.
   */
  object Related {

    /**
     * Creates a query satisfied when any of the specified pointers are present.
     *
     * @tparam T The type of entity that the relation is from.
     * @param relation The relation to query.
     * @param pointers The pointers that satisfy this query.
     * @return A query satisfied when any of the specified pointers are present.
     */
    def apply[T <: AnyRef](
      relation: Relation[T],
      pointers: Pointer[Pointer.Entity[_ <: AnyRef]]*
    ): Related[T] = Related(relation, pointers.toList)

  }

  /**
   * The inverse of the specified query.
   *
   * @tparam T The type of entity that the relation is from.
   * @param query The inverse of the specified query.
   */
  case class Not[T <: AnyRef](query: Query[T]) extends Query[T]

  /**
   * A query that requires both of its subqueries to be satisfied.
   *
   * @tparam T The type of entity that the relation is from.
   * @param first The first subquery to satisfy.
   * @param second The second subquery to satisfy.
   */
  case class And[T <: AnyRef](first: Query[T], second: Query[T]) extends Query[T]

  /**
   * A query that requires either of its subqueries to be satisfied.
   *
   * @tparam T The type of entity that the relation is from.
   * @param first The first subquery to possibly satisfy.
   * @param second The second subquery to possibly satisfy.
   */
  case class Or[T <: AnyRef](first: Query[T], second: Query[T]) extends Query[T]

  /**
   * A query that requires either of its subqueries to be satisfied but not both of them.
   *
   * @tparam T The type of entity that the relation is from.
   * @param first The first subquery to possibly satisfy.
   * @param second The second subquery to possibly satisfy.
   */
  case class Xor[T <: AnyRef](first: Query[T], second: Query[T]) extends Query[T]

}
