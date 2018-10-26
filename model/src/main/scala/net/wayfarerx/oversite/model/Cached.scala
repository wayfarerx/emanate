/*
 * Cached.scala
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

import java.util.concurrent.atomic.AtomicReference

import language.implicitConversions
import ref.SoftReference

import cats.effect.IO

/**
 * Base type for lazy-loaded cached references.
 *
 * @tparam T The type of reference that is cached.
 */
sealed trait Cached[T <: AnyRef] extends (() => IO[T]) {

  /** The type that is kept in the atomic reference. */
  protected type AtomicType

  /** The state of the cache. */
  private val cached = new AtomicReference[Option[AtomicType]](None)

  /** Create the operation that loads the cached reference. */
  private val load: IO[T] = IO(cached.get) flatMap { current =>
    current flatMap decode map IO.pure getOrElse
      create.flatMap(v => if (cached.compareAndSet(current, Some(encode(v)))) IO.pure(v) else load)
  }

  /** Returns the operation that creates a new cached value. */
  protected def create: IO[T]

  /**
   * Encodes a value into an atomic instance.
   *
   * @param value The value to encode.
   * @return The encoded atomic instance.
   */
  protected def encode(value: T): AtomicType

  /**
   * Attempts to decode an atomic instance.
   *
   * @param atomic The atomic instance to decode.
   * @return The decoded value if one is available.
   */
  protected def decode(atomic: AtomicType): Option[T]

  /* Return the operation that loads the cached reference. */
  final override def apply(): IO[T] = load

}

/**
 * Definitions of the supported caching strategies.
 */
object Cached {

  /**
   * Creates a new cached reference.
   *
   * @tparam T The type of reference that is cached.
   * @param create The strategy for providing a new reference to be cached.
   * @param soft   True if the reference should be released to conserve memory.
   * @return A new cached reference.
   */
  def apply[T <: AnyRef](create: IO[T], soft: Boolean = false): Cached[T] =
    if (soft) Soft(create) else Hard(create)

  /**
   * A cached reference that is never released once created.
   *
   * @tparam T The type of reference that is hard-cached.
   * @param create The strategy for providing a new reference to be cached.
   */
  final class Hard[T <: AnyRef](override protected val create: IO[T]) extends Cached[T] {

    /* Use the cached type as the atomic type. */
    override protected type AtomicType = T

    /* Use the cached type as the atomic type. */
    override protected def encode(value: T): AtomicType = value

    /* Use the atomic type as the cached type. */
    override protected def decode(atomic: AtomicType): Option[T] = Some(atomic)

  }

  /**
   * Factory for hard-cached references.
   */
  object Hard {

    /**
     * Creates a new hard-cached reference.
     *
     * @tparam T The type of reference that is cached.
     * @param create The strategy for providing a new reference to be cached.
     * @return A new hard-cached reference.
     */
    def apply[T <: AnyRef](create: IO[T]): Hard[T] = new Hard(create)

  }

  /**
   * A cached reference that may released to conserve memory.
   *
   * @tparam T The type of reference that is soft-cached.
   * @param create The strategy for providing a new reference to be cached.
   */
  final class Soft[T <: AnyRef](override protected val create: IO[T]) extends Cached[T] {

    /* Use the cached type as the atomic type. */
    override protected type AtomicType = SoftReference[T]

    /* Use the cached type as the atomic type. */
    override protected def encode(value: T): AtomicType = SoftReference(value)

    /* Use the atomic type as the cached type. */
    override protected def decode(atomic: AtomicType): Option[T] = atomic.get

  }

  /**
   * Factory for soft-cached references.
   */
  object Soft {

    /**
     * Creates a new soft-cached reference.
     *
     * @tparam T The type of reference that is cached.
     * @param create The strategy for providing a new reference to be cached.
     * @return A new soft-cached reference.
     */
    def apply[T <: AnyRef](create: IO[T]): Soft[T] = new Soft(create)

  }

}
