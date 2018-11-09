/*
 * Node.scala
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

import java.net.URL

import io.{Codec, Source}
import cats.effect.IO
import net.wayfarerx.oversite.Pointer.Image

/**
 * Base type for all nodes.
 *
 * @tparam T The underlying type of entity.
 */
sealed trait Node[T <: AnyRef] extends Context {

  /** Publish this node as the implicit context. */
  final protected implicit def context: Context = this

  //
  // The state of this node.
  //

  /** The identifiers for this node. */
  final private val _identifiers = Cached(loadIdentifiers)

  /** The document loaded for this node. */
  final private val _document = Cached.Soft(Parser parse resource)

  /** The entity decoded for this node. */
  final private val _entity = Cached.Soft(document flatMap scope.decoder.decode)

  //
  // The API for all nodes.
  //

  /** The scope of this node. */
  def scope: Scope[T]

  /** The resource that describes this node. */
  def resource: URL

  /** The resource collection to load from. */
  def resources: Resources

  /** The index of nodes in the site. */
  def index: IO[Index]

  /** Returns the identifiers for this node. */
  final def identifiers: IO[Vector[Name]] = _identifiers()

  /** Returns the document loaded for this node. */
  final def document: IO[Document] = _document()

  /** Returns the entity decoded for this node. */
  final def entity: IO[T] = _entity()

  //
  // The context implementations for all nodes.
  //

  /* Convert the stylesheets into stylesheet references. */
  override def stylesheets: Vector[Scope.Styles.Reference] =
    scope.stylesheets map {
      case Scope.Styles.Generated(name, _) => Scope.Styles.Internal(Pointer.Stylesheet(name))
      case reference: Scope.Styles.Reference => reference
    }

  /* Resolve all pointers. */
  final override def resolve[P <: Pointer.Type](pointer: Pointer[P]): IO[Pointer.Resolved[P]] = pointer match {
    case p: Pointer.Internal[P] => resolve(p)
    case p: Pointer.Resolved[P] => IO.pure(p)
  }

  /* Resolve internal pointers. */
  final override def resolve[P <: Pointer.Type.Aux[S], S](pointer: Pointer.Internal[P]): IO[Pointer.Target[P, S]] =
    pointer.tpe match {
      case e@Pointer.Entity(_) => pointer match {
        case Pointer.Search(_, from, query) => searchForEntity(e, from, query) map { node =>
          Pointer.Target(e, Pointer.Prefix(location, node.location), ()).asInstanceOf[Pointer.Target[P, S]]
        }
        case Pointer.Target(_, at, _) => findEntity(e, at) map { node =>
          Pointer.Target(e, Pointer.Prefix(location, node.location), ()).asInstanceOf[Pointer.Target[P, S]]
        }
      }
      case a: Pointer.Asset =>
        ???
    }

  /* Resolve external pointers. */
  final override def resolve[P <: Pointer.Asset](pointer: Pointer.External[P]): IO[Pointer.External[P]] =
    IO.pure(pointer)

  /* Load the specified entity. */
  final override def load[E <: AnyRef](pointer: Pointer.Internal[Pointer.Entity[E]]): IO[E] = {
    ???
  }

  /* Attempt to load the alt text for an image. */
  final override def alt(image: Pointer.Internal[Image]): IO[Option[String]] =
    resolve(image) map { _ =>
      ???
    }

  //
  // Helper methods.
  //

  /** Loads the identifiers of this node. */
  protected def loadIdentifiers: IO[Vector[Name]] = for {
    e <- IO(Source.fromURL(resource)(Codec.UTF8)).bracket { source =>
      IO(source.getLines.map(_.trim).dropWhile(_.isEmpty).buffered.headOption match {
        case Some(head) if head startsWith "#" => Name(head substring 1).toVector
        case None => Vector.empty
      })
    }(s => IO(s.close()))
  } yield e

  /* Attempts to find the specified entity node. */
  private def findEntity[E <: AnyRef](entity: Pointer.Entity[E], at: Pointer.Prefix): IO[Node[E]] = {
    at match {
      case Pointer.Prefix.Relative(p) => location :++ p
      case Pointer.Prefix.Absolute(l) => Some(l)
    }
  } map (t => index map (i => selectEntity(entity, i(t).toSeq))) getOrElse IO.pure(None) flatMap {
    case Some(r) => IO.pure(r)
    case None => raise(s"${entity.classInfo.getSimpleName} not found at $at")
  }

  /* Attempts to search for the specified entity node. */
  private def searchForEntity[E <: AnyRef](entity: Pointer.Entity[E], from: Pointer.Prefix, query: Name): IO[Node[E]] =
    findEntity(Pointer.Entity.anyRef, from) flatMap { node =>
      index map (i => selectEntity(entity, i(query, node.location))) flatMap {
        case Some(r) => IO.pure(r)
        case None => raise(s"${entity.classInfo.getSimpleName} not found at $from$query")
      }
    }

  private def selectEntity[E <: AnyRef](entity: Pointer.Entity[E], nodes: Seq[Node[_ <: AnyRef]]): Option[Node[E]] =
    nodes collectFirst {
      case n if entity.classInfo.isAssignableFrom(n.scope.classTag.runtimeClass) => n.asInstanceOf[Node[E]]
    }

  private def findAsset[A <: Pointer.Asset](pointer: Pointer.Internal[A]): IO[(Node[_], String)] =
    ???

  private def raise[E](message: String): IO[E] =
    IO.raiseError(new IllegalArgumentException(message))

}

/**
 * Definitions of the node tree.
 */
object Node {

  /**
   * Base type for nodes that contain other nodes.
   *
   * @tparam T The underlying type of entity.
   */
  sealed trait Parent[T <: AnyRef] extends Node[T] {

    /** The cached children of this node. */
    final private val _children = Cached(loadChildren)

    /** The children of this node. */
    final def children: IO[Vector[Child[_ <: AnyRef]]] = _children()

    /** Loads the children of this node. */
    final private def loadChildren: IO[Vector[Child[_ <: AnyRef]]] = {

      def process(remaining: Vector[String]): IO[Vector[Child[_ <: AnyRef]]] = remaining match {
        case head +: tail if head endsWith "/" => for {
          u <- resources.find(location.path + head + "index.md")
          t <- process(tail)
        } yield u flatMap (uu => Name(head) map (uu -> _)) map { case (uu, n) =>
          Branch(this, n, scope(n), uu) +: t
        } getOrElse t
        case head +: tail if head.toLowerCase.endsWith(".md") && !head.equalsIgnoreCase("index.md") =>
          for {
            u <- resources.find(location.path + head)
            t <- process(tail)
          } yield u flatMap (uu => Name(head.substring(0, head.length - 3)) map (uu -> _)) map { case (uu, n) =>
            Leaf(this, n, scope(n), uu) +: t
          } getOrElse t
        case _ +: tail => process(tail)
        case _ => IO.pure(Vector.empty)
      }

      resources.list(location.path.toString) flatMap process
    }

  }


  /**
   * Base type for nodes that are contained by other nodes.
   *
   * @tparam T The underlying type of entity.
   */
  sealed trait Child[T <: AnyRef] extends Node[T] {

    /** The parent of this child node. */
    def parent: Parent[_ <: AnyRef]

    /** The name of this child node. */
    def name: Name

    /* Return the parent's site. */
    final override def site: Site[_ <: AnyRef] = parent.site

    /* The parent's location extended with this child's name. */
    final override lazy val location: Location = parent.location :+ name

    /* Return the parent's stylesheets, mapped to the child's location, and the child's stylesheets. */
    final override lazy val stylesheets: Vector[Scope.Styles.Reference] =
      parent.stylesheets.map {
        case Scope.Styles.Internal(p) => Scope.Styles.Internal(p.scope match {
          case Pointer.Prefix.Relative(path) => p.withPrefix(Pointer.Prefix.Relative(Path.Parent +: path))
          case _ => p
        })
        case other => other
      } ++ super.stylesheets

    /* Use the parent to resolve authors. */
    final override def resolve(author: Author): IO[Author] = parent.resolve(author)

    /* Return the parent's resources. */
    final override def resources: Resources = parent.resources

    /* Return the parent's index. */
    final override def index: IO[Index] = parent.index

    /* Use the loaded identifier and this child's name. */
    final override protected def loadIdentifiers: IO[Vector[Name]] =
      super.loadIdentifiers map (_ :+ name)

  }

  /**
   * The root node in a tree.
   *
   * @tparam T The underlying type of entity.
   */
  case class Root[T <: AnyRef](site: Site[T], resource: URL, resources: Resources) extends Parent[T] {

    /** The index loaded for this node. */
    final private val _index = Cached(Index(this))

    /** Returns the index loaded for this node. */
    override def index: IO[Index] = _index()

    override def location: Location = Location.empty

    override def scope: Scope[T] = site.scopes

    override def resolve(author: Author): IO[Author] = ???

  }

  /**
   * A branch node that both contains and is contained by other nodes.
   *
   * @tparam T The underlying type of entity.
   */
  case class Branch[T <: AnyRef](parent: Parent[_ <: AnyRef], name: Name, scope: Scope[T], resource: URL)
    extends Parent[T] with Child[T]

  /**
   * A leaf node that is contained by other nodes.
   *
   * @tparam T The underlying type of entity.
   */
  case class Leaf[T <: AnyRef](parent: Parent[_ <: AnyRef], name: Name, scope: Scope[T], resource: URL)
    extends Child[T]

}
