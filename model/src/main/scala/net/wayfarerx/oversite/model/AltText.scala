/*
 * AltText.scala
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

import java.util.Properties
import java.util.concurrent.atomic.AtomicReference

import collection.JavaConverters._

import cats.effect.IO

/**
 * A cache for the alternate text associated with images.
 *
 * @param location  The location to search from.
 * @param resources The resource collection to load from.
 * @param resource  The name of the resource to load.
 */
final class AltText(location: Location, resources: Resources, resource: String = "alt.txt") {

  /** The cache of alt-text bundles. */
  private val cache = new AtomicReference(Map.empty: Map[Path, Cached.Soft[Map[Name, String]]])

  /**
   * Attempts to return the alt-text for the specified image.
   *
   * @param path The path to the image.
   * @param file The file name of the image.
   * @return The alt-text for the image if it is available.
   */
  def apply(path: Path, file: String): IO[Option[String]] = {

    /* Attempt to look up the alt-text recursively. */
    def lookup(): IO[Option[String]] = {
      val map = cache.get()
      map get path match {
        case Some(cached) =>
          cached() map { alt => {
            file lastIndexOf '.' match {
              case i if i >= 0 => Name(file.substring(0, i))
              case _ => Name(file)
            }
          } flatMap alt.get
          }
        case None =>
          cache.compareAndSet(map, map + (path -> Cached.Soft {
            resources.find(location.toString + path + resource) flatMap {
              case Some(url) => IO(url.openStream()).bracket { stream =>
                val properties = new Properties
                IO(properties.load(stream)) map { _ =>
                  properties.asScala.flatMap {
                    case (k, v) => Name(k) map (_ -> v)
                  }.toMap
                }
              }(stream => IO(stream.close()))
              case None =>
                IO.pure(Map.empty)
            }
          }))
          lookup()
      }
    }

    lookup()
  }

}
