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

/**
 * Base type for all nodes.
 *
 * @tparam T The underlying type of entity.
 */
sealed trait Node[T <: AnyRef] extends Context {

  /** Publish this node as the implicit context. */
  final protected implicit def context: Context = this

  /** The title of this node. */
  private val _title = Cached(IO(Source.fromURL(resource)(Codec.UTF8)).bracket { source =>
    IO(source.getLines.map(_.trim).dropWhile(_.isEmpty).buffered.headOption match {
      case Some(head) if head startsWith "#" => Name(head substring 1)
      case None => None
    })
  }(s => IO(s.close())))

  /** The document loaded for this node. */
  private val _document = Cached.Soft(Parser parse resource)

  /** The entity decoded for this node. */
  private val _entity = Cached.Soft(document flatMap scope.decoder.decode)

  /** The cache ot alternate text for images. */
  private lazy val _altText = new AltText(location, resources)

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
  def identifiers: IO[Vector[Name]] = title map (_.toVector)

  /** Returns the identifiers for this node. */
  final def title: IO[Option[Name]] = _title()

  /** Returns the document loaded for this node. */
  final def document: IO[Document] = _document()

  /** Returns the entity decoded for this node. */
  final def entity: IO[T] = _entity()

  /**
   * Attempts to return the alt-text for the specified image.
   *
   * @param path The path to the image.
   * @param file The file name of the image.
   * @return The alt-text for the image if it is available.
   */
  final def altText(path: Path, file: String): IO[Option[String]] = _altText(path, file)

  /**
   * Returns true if this node generates the specified file.
   *
   * @param path The path to check for file generation in.
   * @param file The file to check for generation in the path.
   * @return True if this node generates the specified file.
   */
  final def generates(path: Path, file: String): Boolean =
    scope.stylesheets exists {
      case Scope.Styles.Generated(n, _) => path match {
        case Path(Vector(Path.Child(p))) if p == Pointer.Stylesheet.prefix =>
          file == s"$n.${Pointer.Stylesheet.extension}"
        case _ => false
      }
      case _ => false
    }

  /**
   * Returns true if this node generates the specified file.
   *
   * @param asset The asset type to check for generation.
   * @param name  The name to check for generation.
   * @return True if this node generates the specified file.
   */
  final def generates(asset: Pointer.Asset, name: Name): Boolean =
    asset == Pointer.Stylesheet && scope.stylesheets.exists {
      case Scope.Styles.Generated(n, _) => n == name
      case _ => false
    }

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
          Pointer.Target(e.asInstanceOf[P], Pointer.Prefix(location, node.location), ().asInstanceOf[S])
        }
        case Pointer.Target(_, at, _) => targetEntity(e, at) map { node =>
          Pointer.Target(e.asInstanceOf[P], Pointer.Prefix(location, node.location), ().asInstanceOf[S])
        }
      }
      case a: Pointer.Asset => pointer match {
        case Pointer.Search(_, from, query) => searchForAsset(a, from, query) map { case (n, p, s) =>
          Pointer.Target(a.asInstanceOf[P], Pointer.Prefix(location, n.location), (p.toString + s).asInstanceOf[S])
        }
        case Pointer.Target(_, prefix, suffix) => targetAsset(prefix, suffix.toString) map { case (n, p, s) =>
          Pointer.Target(a.asInstanceOf[P], Pointer.Prefix(location, n.location), (p.toString + s).asInstanceOf[S])
        }
      }
    }

  /* Resolve external pointers. */
  final override def resolve[P <: Pointer.Asset](pointer: Pointer.External[P]): IO[Pointer.External[P]] =
    IO.pure(pointer)

  /* Load the specified entity. */
  final override def load[E <: AnyRef](pointer: Pointer.Internal[Pointer.Entity[E]]): IO[E] = pointer match {
    case Pointer.Search(_, from, query) =>
      searchForEntity(pointer.tpe, from, query) flatMap (_.entity map (_.asInstanceOf[E]))
    case Pointer.Target(_, at, _) =>
      targetEntity(pointer.tpe, at) flatMap (_.entity map (_.asInstanceOf[E]))
  }

  /* Attempt to load the alt text for an image. */
  final override def alt(image: Pointer.Internal[Pointer.Image]): IO[Option[String]] = (image match {
    case Pointer.Search(_, from, query) =>
      searchForAsset(image.tpe, from, query)
    case Pointer.Target(_, at, file) =>
      targetAsset(at, file.toString)
  }) flatMap { case (node, path, file) => node.altText(path, file) }

  //
  // Helper methods.
  //

  /**
   * Targets the specified entity node.
   *
   * @param entity The type of entity node to find.
   * @param at     The prefix that specifies the desired entity node.
   * @return The desired entity node.
   */
  private def targetEntity(
    entity: Pointer.Entity[_ <: AnyRef],
    at: Pointer.Prefix
  ): IO[Node[_ <: AnyRef]] =
    locate(at) map (l => index map (i => selectEntity(entity, i(l).toSeq))) getOrElse IO.pure(None) flatMap {
      case Some(r) => IO.pure(r)
      case None => raise(s"Entity ${entity.classInfo.getSimpleName} not found at $at")
    }

  /**
   * Searches for the specified entity node.
   *
   * @param entity The type of entity node to find.
   * @param from   The prefix that specifies the node to search from.
   * @return The desired entity node.
   */
  private def searchForEntity(
    entity: Pointer.Entity[_ <: AnyRef],
    from: Pointer.Prefix,
    query: Name
  ): IO[Node[_ <: AnyRef]] =
    targetEntity(Pointer.Entity[AnyRef], from) flatMap { node =>
      index map (idx => selectEntity(entity, idx(query, node.location))) flatMap {
        case Some(r) => IO.pure(r)
        case None => raise(s"Entity ${entity.classInfo.getSimpleName} not found at $from$query")
      }
    }

  /**
   * Selects the first node that is assignable to the specified entity.
   *
   * @param entity The entity to assign to.
   * @param nodes  The nodes to assign from.
   * @return The first node that is assignable to the specified entity.
   */
  private def selectEntity(
    entity: Pointer.Entity[_ <: AnyRef],
    nodes: Seq[Node[_ <: AnyRef]]
  ): Option[Node[_ <: AnyRef]] =
    nodes find (n => entity.classInfo.isAssignableFrom(n.scope.classTag.runtimeClass))

  /**
   * Targets the specified asset.
   *
   * @param prefix The prefix of the targeted asset.
   * @param suffix The suffix of the targeted asset.
   * @return The desired asset target.
   */
  private def targetAsset(
    prefix: Pointer.Prefix,
    suffix: String
  ): IO[(Node[_ <: AnyRef], Path, String)] =
    canonicalAsset(prefix, suffix) flatMap { case (node, path, file) =>
      if (node.generates(path, file)) IO.pure((node, path, file))
      else resources find node.location.toString + path + file flatMap {
        _ map (_ => IO.pure((node, path, file))) getOrElse[IO[(Node[_ <: AnyRef], Path, String)]]
          raise(s"Cannot find asset ${node.location}$path$file")
      }
    }

  /**
   * Searches for the specified asset.
   *
   * @param asset The type of asset to find.
   * @param from  The prefix that specifies the node to search from.
   * @param query The file name query to search for.
   * @return The desired asset target.
   */
  private def searchForAsset(
    asset: Pointer.Asset,
    from: Pointer.Prefix,
    query: Name
  ): IO[(Node[_ <: AnyRef], Path, String)] =
    canonicalAsset(from, "") flatMap { case (start, path, _) =>

      /* Search for all the specified extensions. */
      def searchExtensions(node: Node[_ <: AnyRef], remaining: Vector[String]): IO[Option[String]] =
        remaining match {
          case head +: tail =>
            val file = s"$query.$head"
            if (node.generates(path, file)) IO.pure(Some(file))
            else resources find node.location.toString + path + file flatMap {
              _ map (_ => IO.pure(Some(file))) getOrElse searchExtensions(node, tail)
            }
          case _ => IO.pure(None)
        }

      /* Search the specified node and all of its parents. */
      def searchUpwards(node: Node[_ <: AnyRef]): IO[Option[(Node[_ <: AnyRef], Path, String)]] =
        searchExtensions(node, asset.extensions.toVector) flatMap {
          case Some(file) =>
            IO.pure(Some((node, path, file)))
          case None =>
            if (path.elements.isEmpty && node.generates(asset, query)) {
              IO.pure(Some((node, Path(asset.prefix), s"$query.${asset.extensions.head}")))
            } else node match {
              case child: Node.Child[_] => searchUpwards(child.parent)
              case _ => IO.pure(None)
            }
        }

      searchUpwards(start) flatMap {
        case Some(result) => IO.pure(result)
        case None => raise(s"Cannot find ${asset.default} asset $from?$path")
      }
    }

  /**
   * Converts an asset target into a canonical node and resource path.
   *
   * @param prefix The prefix of the asset.
   * @param suffix The suffix of the asset.
   * @return The contextualized node and resource path for the targeted asset.
   */
  private def canonicalAsset(prefix: Pointer.Prefix, suffix: Path.Regular): IO[(Node[_ <: AnyRef], Path, String)] = {

    /* Search for a context recursively. */
    @annotation.tailrec
    def contextualize(idx: Index, at: Location, path: Path): (Node[_ <: AnyRef], Path) = idx(at) match {
      case Some(n) => n -> path
      case None => contextualize(idx, at.parent.get, at.path.elements.last +: path)
    }

    locate(prefix) flatMap { l =>
      suffix lastIndexOf '/' match {
        case i if i >= 0 => l :++ Path(suffix.substring(0, i)) map (_ -> suffix.substring(i + 1))
        case _ => Some(l -> suffix.string)
      }
    } map { case (at, file) =>
      index map { idx =>
        val (node, path) = contextualize(idx, at, Path.empty)
        (node, path, file)
      }
    } getOrElse raise(s"Unable to contextualize asset $prefix$suffix")
  }

  /**
   * Returns the location for a prefix.
   *
   * @param prefix The prefix to return the location for.
   * @return The location if the prefix is valid.
   */
  private def locate(prefix: Pointer.Prefix): Option[Location] = prefix match {
    case Pointer.Prefix.Relative(p) => location :++ p
    case Pointer.Prefix.Absolute(l) => Some(l)
  }

  /**
   * Raises an error IO.
   *
   * @param message The error message.
   * @return An error IO.
   */
  private def raise(message: String): IO[Nothing] =
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
    final override def identifiers: IO[Vector[Name]] = super.identifiers map (name +: _)

  }

  /**
   * The root node in a tree.
   *
   * @tparam T The underlying type of entity.
   * @param site      The site behind this root node.
   * @param resource  The root resource.
   * @param resources The resource collection to load from.
   */
  case class Root[T <: AnyRef](site: Site[T], resource: URL, resources: Resources) extends Parent[T] {

    /** The authors loaded for all nodes. */
    private val _authors = Cached(Authors(resources))

    /** The index loaded for all nodes. */
    private val _index = Cached(Index(this))

    /* Return the index loaded for this node. */
    override def index: IO[Index] = _index()

    /* Always at the empty location. */
    override def location: Location = Location.empty

    /* Return the root scope. */
    override def scope: Scope[T] = site.scopes

    /* Resolve an author in the site. */
    override def resolve(author: Author): IO[Author] = _authors() map (_ (author.name) getOrElse author)

  }

  /**
   * A branch node that both contains and is contained by other nodes.
   *
   * @tparam T The underlying type of entity.
   * @param parent The parent of this branch.
   * @param name   The name of this branch.
   * @param scope  The scope of this branch.
   */
  case class Branch[T <: AnyRef](parent: Parent[_ <: AnyRef], name: Name, scope: Scope[T], resource: URL)
    extends Parent[T] with Child[T]

  /**
   * A leaf node that is contained by other nodes.
   *
   * @tparam T The underlying type of entity.
   * @param parent The parent of this branch.
   * @param name   The name of this branch.
   * @param scope  The scope of this branch.
   */
  case class Leaf[T <: AnyRef](parent: Parent[_ <: AnyRef], name: Name, scope: Scope[T], resource: URL) extends Child[T]

}
