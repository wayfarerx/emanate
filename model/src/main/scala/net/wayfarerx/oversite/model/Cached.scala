/*
 * Cached.scala
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
package model

import java.util.concurrent.atomic.AtomicReference

import language.implicitConversions
import ref.SoftReference

import cats.effect.IO

/**
 * Factory for cached effects.
 */
object Cached {

  /**
   * Creates a new cached effect.
   *
   * @tparam T The type of reference that is cached.
   * @param create The strategy for providing the reference to be cached.
   * @param soft   True if the reference should be released to conserve memory, defaults to `false`.
   * @return A new cached effect.
   */
  def apply[T <: AnyRef](create: IO[T], soft: Boolean = false): IO[T] =
    if (soft) Soft(create) else Hard(create)

  /**
   * Factory for hard-cached effects.
   */
  object Hard {

    /**
     * Creates a new hard-cached effect.
     *
     * @tparam T The type that is cached.
     * @param create The strategy for providing the data to be cached.
     * @return A new hard-cached effect.
     */
    def apply[T](create: IO[T]): IO[T] = {
      val cached = new AtomicReference(None: Option[T])

      /* Recursively load the cached data. */
      def load: IO[T] = IO(cached.get) flatMap {
        _ map IO.pure getOrElse create.flatMap { v =>
          if (cached.compareAndSet(None, Some(v))) IO.pure(v) else load
        }
      }

      load
    }

  }

  /**
   * Factory for soft-cached effects.
   */
  object Soft {

    /**
     * Creates a new soft-cached effect.
     *
     * @tparam T The type of reference that is cached.
     * @param create The strategy for providing the reference to be cached.
     * @return A new soft-cached effect.
     */
    def apply[T <: AnyRef](create: IO[T]): IO[T] = {
      val cached = new AtomicReference(SoftReference(None: Option[T]))

      /* Recursively load the cached reference. */
      def load: IO[T] = IO(cached.get) flatMap { ref =>
        ref.get.flatten map IO.pure getOrElse create.flatMap { v =>
          if (cached.compareAndSet(ref, SoftReference(Some(v)))) IO.pure(v) else load
        }
      }

      load
    }

  }

  /**
   * Factory for maps of cached effects.
   */
  object Mapped {

    /**
     * Creates a new map of cached effects.
     *
     * @tparam K The type of key to map by.
     * @tparam V The type of reference to cache.
     * @param create The strategy for providing the references to be cached.
     * @param soft   True if the references should be released to conserve memory, defaults to `false`.
     * @return A new map of cached effects.
     */
    def apply[K, V <: AnyRef](create: K => IO[Option[V]], soft: Boolean = false): K => IO[Option[V]] =
      if (soft) Soft(create) else Hard(create)

    /**
     * Factory for maps of hard-cached effects.
     */
    object Hard {

      /**
       * Creates a new map of hard-cached effects.
       *
       * @tparam K The type of key to map by.
       * @tparam V The type of data to cache.
       * @param create The strategy for providing the data to be cached.
       * @return A new map of hard-cached effects.
       */
      def apply[K, V](create: K => IO[Option[V]]): K => IO[Option[V]] = {
        val cached = new AtomicReference(Map.empty[K, V])

        /* Recursively load a cached entry. */
        def load(key: K): IO[Option[V]] = for {
          map <- IO(cached.get)
          outcome <- map get key map (v => IO pure map -> Some(v)) getOrElse
            create(key).map(_ map (vv => map + (key -> vv) -> Some(vv)) getOrElse map -> None)
          result <- if (cached.compareAndSet(map, outcome._1)) IO.pure(outcome._2) else load(key)
        } yield result

        load
      }

    }

    /**
     * Factory for maps of soft-cached effects.
     */
    object Soft {

      /**
       * Creates a new map of soft-cached effects.
       *
       * @tparam K The type of key to map by.
       * @tparam V The type of reference to cache.
       * @param create The strategy for providing the data to be cached.
       * @return A new map of soft-cached effects.
       */
      def apply[K, V <: AnyRef](create: K => IO[Option[V]]): K => IO[Option[V]] = {
        val cached = new AtomicReference(Map.empty[K, SoftReference[V]])

        /* Recursively load a cached entry. */
        def load(key: K): IO[Option[V]] = for {
          map <- IO(cached.get)
          outcome <- map get key flatMap (_.get) map (v => IO pure map -> Some(v)) getOrElse
            create(key).map(_ map (vv => map + (key -> SoftReference(vv)) -> Some(vv)) getOrElse map -> None)
          result <- if (cached.compareAndSet(map, outcome._1)) IO.pure(outcome._2) else load(key)
        } yield result

        load
      }

    }

  }

}
