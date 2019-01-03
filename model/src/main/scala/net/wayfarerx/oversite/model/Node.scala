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
import java.util.concurrent.atomic.AtomicReference

import language.existentials
import util.control.NoStackTrace
import io.{Codec, Source}
import cats.effect.IO

import scala.reflect.ClassTag

/**
 * Base type for all nodes.
 *
 * @tparam T The underlying type of entity.
 */
sealed trait Node[T <: AnyRef] extends Context {

  import Node._

  /** Publish this node as the implicit context. */
  final protected implicit def context: Context = this

  /** The title of this node. */
  final val title: IO[Name] = Cached(IO(Source.fromURL(resource)(Codec.UTF8)).bracket { source =>
    IO(source.getLines.map(_.trim).dropWhile(_.isEmpty).buffered.headOption) flatMap (_.collect {
      case head if head.startsWith("#") && !head.startsWith("##") => Name(head substring 1)
    }.flatten map IO.pure getOrElse Problem.raise(s"Unable to load title from $location"))
  }(s => IO(s.close())))()

  /** The document loaded for this node. */
  final val document: IO[Document] = Cached.Soft(Parser parse resource)()

  /** The entity decoded for this node. */
  final val entity: IO[T] = Cached.Soft(document flatMap scope.decoder.decode)()

  /** The cache ot alternate text for images. */
  private lazy val _altText = new AltText(location, resources)

  /** The cache of the entity's relations. */
  private val _relations = new AtomicReference(Map.empty: Map[Relation[T], Cached[Set[Node[_ <: AnyRef]]]])

  //
  // The API for all nodes.
  //

  /** The scope of this node. */
  def scope: Scope[T]

  /** The path to the resource. */
  def source: Path

  /** The resource that describes this node. */
  def resource: URL

  /** The resource collection to load from. */
  def resources: Resources

  /** The index of nodes in the site. */
  def index: IO[Index]

  /** Returns the identifiers for this node. */
  def identifiers: IO[Vector[Name]] = title map (Vector(_))

  /**
   * Attempts to return the alt-text for the specified image.
   *
   * @param path The path to the image.
   * @param file The file name of the image.
   * @return The alt-text for the image if it is available.
   */
  final def altText(path: Path, file: String): IO[Option[String]] =
    _altText(path, file)

  /**
   * Returns the nodes related to this node.
   *
   * @param relation The relation that identifies related nodes.
   * @return The nodes related to this node.
   */
  final def relations(relation: Relation[T]): IO[Set[Node[_ <: AnyRef]]] = {

    @annotation.tailrec
    def lookup(): IO[Set[Node[_ <: AnyRef]]] = {
      val map = _relations.get()
      map get relation match {
        case Some(cached) =>
          cached()
        case None =>
          _relations.compareAndSet(map, map + (relation -> Cached {
            entity map (relation(_).toVector) flatMap resolveAll map (_.toSet)
          }))
          lookup()
      }
    }

    lookup()
  }

  /**
   * Returns true if this node generates the specified file.
   *
   * @param path The path to check for file generation in.
   * @param file The file to check for generation in the path.
   * @return True if this node generates the specified file.
   */
  final def generates(path: Path, file: String): Boolean = {
    for {
      p <- path match {
        case Path(Vector(Path.Child(pp))) => Some(Some(pp))
        case Path.empty => Some(None)
        case _ => None
      }
      (n, e) <- file lastIndexOf '.' match {
        case index if index >= 0 =>
          Name(file.substring(0, index)) flatMap (nn => Name(file.substring(index + 1)) map (nn -> _))
        case _ => None
      }
      v <- Pointer.VariantsByExtension get e
    } yield v.asset.prefix == p && scope.generators.exists {
      case Scope.Generator(nn, vv, _) => n == nn && v == vv
    }
  } getOrElse false

  /**
   * Resolves all the specified pointers into nodes.
   *
   * @param remaining The pointers to resolve.
   * @return The resolved nodes.
   */
  final def resolveAll(remaining: Vector[Pointer[Pointer.Entity[_ <: AnyRef]]]): IO[Vector[Node[_ <: AnyRef]]] =
    remaining match {
      case head +: tail => for {
        h <- resolve(head)
        t <- resolveAll(tail)
        r <- h match {
          case Pointer.Target(_, prefix, _) =>
            index map (i => prefix toLocation location flatMap (i(_)) map (_ +: t) getOrElse t)
          case _ =>
            sys.error("unreachable")
        }
      } yield r
      case _ =>
        IO.pure(Vector.empty)
    }

  //
  // The context implementations for all nodes.
  //

  /* Resolve all pointers. */
  final override def resolve[P <: Pointer.Type](pointer: Pointer[P]): IO[Pointer.Resolved[P]] =
    pointer match {
      case p: Pointer.Internal[P] => resolveInternal(p)
      case p: Pointer.Resolved[P] => IO.pure(p)
    }

  /* Resolve internal pointers. */
  final override def resolve[P <: Pointer.Type.Aux[S], S](pointer: Pointer.Internal[P]): IO[Pointer.Target[P, S]] =
    resolveInternal(pointer) map (_.asInstanceOf[Pointer.Target[P, S]])

  /* Resolve internal pointers. */
  private def resolveInternal[P <: Pointer.Type](pointer: Pointer.Internal[P]): IO[Pointer.Resolved[P]] =
    pointer.tpe match {
      case e@Pointer.Entity(_) => pointer match {
        case Pointer.Search(_, from, query) => searchForEntity(e, from, query) map {
          node =>
            Pointer.Target(Pointer.Entity(node.scope.classTag.runtimeClass), Pointer.Prefix(location, node.location), ())
              .asInstanceOf[Pointer.Resolved[P]]
        }
        case Pointer.Target(_, at, _) => targetEntity(e, at) map {
          node =>
            Pointer.Target(Pointer.Entity(node.scope.classTag.runtimeClass), Pointer.Prefix(location, node.location), ())
              .asInstanceOf[Pointer.Resolved[P]]
        }
      }
      case a: Pointer.Asset => pointer match {
        case Pointer.Search(_, from, query) => searchForAsset(a, from, query) map {
          case (n, p, s) =>
            Pointer.Target(a, Pointer.Prefix(location, n.location), p.toString + s).asInstanceOf[Pointer.Resolved[P]]
        }
        case Pointer.Target(_, prefix, suffix) => targetAsset(prefix, suffix.toString) map {
          case (n, p, s) =>
            Pointer.Target(a, Pointer.Prefix(location, n.location), p.toString + s).asInstanceOf[Pointer.Resolved[P]]
        }
      }
    }

  /* Resolve external pointers. */
  final override def resolve[P <: Pointer.Asset](pointer: Pointer.External[P]): IO[Pointer.External[P]] =
    IO.pure(pointer)

  /* Search for entities. */
  final override def search[E <: AnyRef : ClassTag](): IO[Vector[Pointer.Resolved[Pointer.Entity[E]]]] =
    index flatMap (_.apply[E](location, _ => IO.pure(true))) map (_ map { node =>
      Pointer.Target(Pointer.Entity[E], Pointer.Prefix(location, node.location), ())
    })

  /* Search for entities. */
  final override def search[E <: AnyRef : ClassTag](
    query: Query[_ >: E]
  ): IO[Vector[Pointer.Resolved[Pointer.Entity[E]]]] = {

    def queryToFunction(query: Query[_ >: E]): Node[_ <: E] => IO[Boolean] = query match {
      case Query.Related(relation, pointers) =>
        node =>
          for {
            r <- node.relations(relation)
            q <- resolveAll(pointers)
          } yield q map (_.location) exists (r map (_.location))
      case Query.Not(subquery) =>
        val s = queryToFunction(subquery)
        node =>
          for (ss <- s(node)) yield !ss
      case Query.And(first, second) =>
        val f = queryToFunction(first)
        val s = queryToFunction(second)
        node =>
          for {
            ff <- f(node)
            ss <- s(node)
          } yield ff && ss
      case Query.Or(first, second) =>
        val f = queryToFunction(first)
        val s = queryToFunction(second)
        node =>
          for {
            ff <- f(node)
            ss <- s(node)
          } yield ff || ss
      case Query.Xor(first, second) =>
        val f = queryToFunction(first)
        val s = queryToFunction(second)
        node =>
          for {
            ff <- f(node)
            ss <- s(node)
          } yield ff ^ ss
    }

    index flatMap (_.apply[E](location, queryToFunction(query))) map (_ map { node =>
      Pointer.Target(Pointer.Entity[E], Pointer.Prefix(location, node.location), ())
    })
  }

  /* Load the specified entity. */
  final override def load[E <: AnyRef](pointer: Pointer[Pointer.Entity[E]]): IO[E] = pointer match {
    case Pointer.Search(_, from, query) =>
      searchForEntity(pointer.tpe, from, query) flatMap (_.entity map (_.asInstanceOf[E]))
    case Pointer.Target(_, at, _) =>
      targetEntity(pointer.tpe, at) flatMap (_.entity map (_.asInstanceOf[E]))
    case _ =>
      sys.error("unreachable")
  }

  /* Attempt to load the alt text for an image. */
  final override def alt(image: Pointer.Internal[Pointer.Image]): IO[Option[String]] = (image match {
    case Pointer.Search(_, from, query) =>
      searchForAsset(image.tpe, from, query)
    case Pointer.Target(_, at, file) =>
      targetAsset(at, file.toString)
  }) flatMap {
    case (node, path, file) => node.altText(path, file)
  }

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
      case None => Problem.raise(s"Entity ${
        entity.classInfo.getSimpleName
      } not found at $at")
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
    targetEntity(Pointer.Entity[AnyRef], from) flatMap {
      node =>
        index map (idx => selectEntity(entity, idx(node.location, query))) flatMap {
          case Some(r) => IO.pure(r)
          case None => Problem.raise(s"Entity ${
            entity.classInfo.getSimpleName
          } not found at $from$query")
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
    canonicalAsset(prefix, suffix) flatMap {
      case (node, path, file) =>
        if (node.generates(path, file)) IO.pure((node, path, file))
        else resources find node.source.toString + path + file flatMap {
          _ map (_ => IO.pure((node, path, file))) getOrElse[IO[(Node[_ <: AnyRef], Path, String)]]
            Problem.raise(s"Cannot find asset ${
              node.location
            }$path$file")
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
  ): IO[(Node[_ <: AnyRef], Path, String)] = {
    canonicalAsset(from, Path.Regular(asset.prefix map (p => s"$p/$query") getOrElse query.normal))
      .flatMap {
        case (start, path, _) =>

          /* Find from the specified extensions. */
          def finding(node: Node[_ <: AnyRef], remaining: Vector[Name]): IO[Option[String]] = remaining match {
            case head +: tail =>
              val file = s"$query.$head"
              if (node.generates(path, file)) IO.pure(Some(file))
              else resources find node.source.toString + path + file flatMap {
                _ map (_ => IO.pure(Some(file))) getOrElse finding(node, tail)
              }
            case _ => IO.pure(None)
          }

          /* Search the specified node and all of its parents. */
          def searching(node: Node[_ <: AnyRef]): IO[Option[(Node[_ <: AnyRef], Path, String)]] =
            finding(node, asset.variants.flatMap(_.extensions).toVector) flatMap {
              case Some(file) =>
                IO.pure(Some((node, path, file)))
              case None =>
                node match {
                  case child: Node.Child[_] => searching(child.parent)
                  case _ => IO.pure(None)
                }
            }

          searching(start) flatMap {
            case Some(result) => IO.pure(result)
            case None => Problem.raise(s"Cannot find ${
              asset.name
            } asset $from?$query")
          }
      }
  }

  /**
   * Converts an asset target into a canonical node, path and file.
   *
   * @param prefix The prefix of the asset.
   * @param suffix The suffix of the asset.
   * @return The contextualized node, path and file for the targeted asset.
   */
  private def canonicalAsset(prefix: Pointer.Prefix, suffix: Path.Regular): IO[(Node[_ <: AnyRef], Path, String)] = {

    /* Search for a context recursively. */
    @annotation.tailrec
    def contextualize(idx: Index, at: Location, path: Path): (Node[_ <: AnyRef], Path) = idx(at) match {
      case Some(n) => n -> path
      case None => contextualize(idx, at.parent.get, at.path.elements.last +: path)
    }

    locate(prefix) flatMap {
      loc =>
        suffix lastIndexOf '/' match {
          case i if i >= 0 => loc :++ Path(suffix.substring(0, i)) map (_ -> suffix.substring(i + 1))
          case _ => Some(loc -> suffix.string)
        }
    } map {
      case (at, file) =>
        index map {
          idx =>
            val (node, path) = contextualize(idx, at, Path.empty)
            (node, path, file)
        }
    } getOrElse Problem.raise(s"Unable to contextualize asset $prefix$suffix")
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

}

/**
 * Definitions of the node tree.
 */
object Node {

  /** The name of the file that describes a parent node. */
  val IndexFile: String = "index.md"

  /**
   * Base type for nodes that contain other nodes.
   *
   * @tparam T The underlying type of entity.
   */
  sealed trait Parent[T <: AnyRef] extends Node[T] {

    /** The children of this node. */
    final val children: IO[Vector[Child[_ <: AnyRef]]] = Cached {

      def findNested(remaining: Vector[String]): IO[Vector[Child[_ <: AnyRef]]] = remaining match {
        case head +: tail if head.endsWith(".md") && !(head == IndexFile || head.endsWith(s"/$IndexFile")) =>
          for {
            u <- resources.find(head)
            t <- findNested(tail)
          } yield u.flatMap(d => Name(head substring head.lastIndexOf('/') + 1 dropRight 3).map(d -> _)) flatMap {
            case (d, n) => scope(n) match {
              case s@Scope.Nested(_, _, _) => Some(Leaf(this, n, s, d) +: t)
              case _ => None
            }
          } getOrElse t
        case head +: tail =>
          val path = Path(head)
          for {
            u <- resources.find(path + IndexFile)
            t <- findNested(tail)
          } yield u flatMap (d => Name(path.elements.last.toString) map (d -> _)) flatMap {
            case (d, n) => scope(n) match {
              case s@Scope.Nested(_, _, _) => Some(Branch(this, n, s, d) +: t)
              case _ => None
            }
          } getOrElse t
        case _ => IO.pure(Vector.empty)
      }

      def findAliased(remaining: Vector[(Name, Scope.Aliased[_ <: AnyRef])]): IO[Vector[Child[_ <: AnyRef]]] =
        remaining match {
          case (name, head) +: tail => for {
            u <- resources.find(head.path + IndexFile)
            t <- findAliased(tail)
          } yield u map (Branch(this, name, head, _) +: t) getOrElse t
          case _ => IO.pure(Vector.empty)
        }

      for {
        nested <- resources.list(source.toString) flatMap findNested
        aliased <- findAliased(scope.children collect {
          case (Scope.Select.Matching(name), s@Scope.Aliased(_, _, _, _)) => name -> s
        })
      } yield nested ++ aliased
    }()

  }


  /**
   * Base type for nodes that are contained by other nodes.
   *
   * @tparam T The underlying type of entity.
   */
  sealed trait Child[T <: AnyRef] extends Node[T] {

    /* The parent's location extended with this child's name. */
    final override lazy val location: Location = parent.location :+ name

    /** The parent of this child node. */
    def parent: Parent[_ <: AnyRef]

    /** The name of this child node. */
    def name: Name

    /* Return the parent's site. */
    final override def site: Site[_ <: AnyRef] = parent.site

    /* Determine the source. */
    final override def source: Path = scope match {
      case Scope.Nested(_, _, _) => parent.source :+ name
      case Scope.Aliased(path, _, _, _) => path
    }

    /* Return the parent's resources. */
    final override def resources: Resources = parent.resources

    /* Return the parent's index. */
    final override def index: IO[Index] = parent.index

    /* Use the loaded identifier and this child's name. */
    final override def identifiers: IO[Vector[Name]] = super.identifiers map (name +: _)

    /* Use the parent to resolve authors. */
    final override def resolve(author: Author): IO[Author] = parent.resolve(author)

  }

  /**
   * The root node in a tree.
   *
   * @tparam T The underlying type of entity.
   * @param site      The site behind this root node.
   * @param resource  The root resource.
   * @param resources The resource collection to load from.
   */
  case class Root[T <: AnyRef] private(site: Site[T], resource: URL, resources: Resources) extends Parent[T] {

    /** The authors loaded for all nodes. */
    private val authors = Cached(Authors(resources))()

    /* Return the index loaded for this node. */
    override val index: IO[Index] = Cached(Index(this))()

    /* Always at the empty location. */
    override def location: Location = Location.empty

    /* Return the root scope. */
    override def scope: Scope[T] = site.scopes

    /* Determine the root source. */
    override def source: Path = scope match {
      case Scope.Nested(_, _, _) => Path.empty
      case Scope.Aliased(path, _, _, _) => path
    }

    /* Resolve an author in the site. */
    override def resolve(author: Author): IO[Author] = authors map (_ (author.name) getOrElse author)

  }

  /**
   * Factory for root nodes.
   */
  object Root {

    /**
     * Attempts to create a root node from the specified site implementation and the context class loader.
     *
     * @tparam T The type of entity the site publishes.
     * @param siteImplementation The name of the site implementation to use.
     * @return The result of attempting to create a root node from the specified site site implementation.
     */
    def apply[T <: AnyRef](siteImplementation: String): IO[Root[T]] =
      apply(siteImplementation, None)

    /**
     * Attempts to create a root node from the specified site implementation and the specified resources.
     *
     * @tparam T The type of entity the site publishes.
     * @param siteImplementation The name of the site implementation to use.
     * @param resources          The resources to use.
     * @return The result of attempting to create a root node from the specified site site implementation.
     */
    def apply[T <: AnyRef](siteImplementation: String, resources: Resources): IO[Root[T]] =
      apply(siteImplementation, Some(resources))

    /**
     * Attempts to create a root node from the specified input and resources.
     *
     * @tparam T The type of entity the site publishes.
     * @param siteImplementation The input to use.
     * @param resources          The resources to use.
     * @return The result of attempting to create a root node from the specified site and resources.
     */
    def apply[T <: AnyRef](siteImplementation: String, resources: Option[Resources]): IO[Root[T]] =
      for {
        res <- IO {
          resources orElse {
            Option(Thread.currentThread.getContextClassLoader)
              .orElse(Option(getClass.getClassLoader))
              .map(Resources.Classpath(_))
          } getOrElse Resources.Default
        }
        site <- res.load(siteImplementation) flatMap (c => IO(c.newInstance.asInstanceOf[Site[T]]))
        resource <- (site.scopes match {
          case Scope.Nested(_, _, _) => res.find(IndexFile)
          case Scope.Aliased(p, _, _, _) => res.find(p + IndexFile)
        }) flatMap (_ map IO.pure getOrElse Problem.raise("Unable to find root index file."))
      } yield Root(site, resource, res)

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

  /**
   * The type of exception produced when parsing problems are encountered.
   *
   * @param message The message that describes this problem.
   */
  final class Problem(message: String) extends RuntimeException(message) with NoStackTrace

  /**
   * Factory for parsing problems.
   */
  object Problem {

    /**
     * Creates a new problem.
     *
     * @param message The message that describes the new problem.
     * @return A new problem.
     */
    def apply(message: String): Problem =
      new Problem(message)

    /**
     * Raises a new problem.
     *
     * @tparam T The type of the result.
     * @param message The message that describes the new problem.
     * @return The raising of a new problem.
     */
    def raise[T](message: String): IO[T] =
      IO.raiseError(Problem(message))

  }

}
