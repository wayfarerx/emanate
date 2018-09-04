/*
 * Authors.scala
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

import io.Source

import cats.effect.IO

/**
 * An index of author information for a website.
 *
 * @param authors The index of author information for a website.
 */
case class Authors private(authors: Map[Name, Author]) {

  /**
   * Returns the author with the specified name.
   *
   * @param name The name of the author to return.
   * @return The author with the specified name.
   */
  def apply(name: String): Option[Author] =
    Name(name) flatMap apply

  /**
   * Returns the author with the specified name.
   *
   * @param name The name of the author to return.
   * @return The author with the specified name.
   */
  def apply(name: Name): Option[Author] =
    authors get name

}

/**
 * Factory for indexed author information for a website.
 */
object Authors {

  def apply(environment: Environment, resource: String): IO[Authors] =
    for {
      url <- environment.find(resource)
      lines <- url map { data =>
        IO(Source.fromInputStream(data.openStream())).bracket(s => IO(s.getLines().toVector))(s => IO(s.close()))
      } getOrElse IO.pure(Vector.empty)
    } yield
      Authors(lines.map(_.split("""[\s]+""").toVector.filterNot(_.isEmpty)).flatMap {
        case Vector(name) =>
          Name(name) map (Author(_))
        case Vector(name, twitterOrEmail) =>
          Name(name) flatMap (n => Some(twitterOrEmail) collect {
            case twitter if twitter.startsWith("@") && twitter.length > 1 => Author(n, Some(twitter drop 1))
            case email if email.contains("@") && email.length > 2 => Author(n, None, Some(email))
          })
        case Vector(name, twitter, email) if twitter.startsWith("@") && email.contains("@") =>
          Name(name) map (Author(_, Some(twitter drop 1) filterNot (_.isEmpty), Some(email) filterNot (_.isEmpty)))
        case _ => None
      }.map(a => a.name -> a).toMap)

}
