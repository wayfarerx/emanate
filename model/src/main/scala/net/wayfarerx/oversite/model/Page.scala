/*
 * Page.scala
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
import java.util.Properties
import java.util.concurrent.ConcurrentHashMap

import collection.JavaConverters._
import io.{Codec, Source}
import ref.SoftReference
import reflect.ClassTag

import cats.effect.IO

import net.wayfarerx.oversite.Asset.Image


/**
 * Base class for a page in the tree of pages that make up a site.
 *
 * @tparam T The type of entity this page represents.
 */
sealed trait Page[T <: AnyRef] extends Context {

  /** The cached title. */
  @volatile
  private var cachedTitles: Option[Vector[Name]] = None

  /** The cached document. */
  @volatile
  private var cachedDocument: Option[SoftReference[Markup.Document]] = None

  /** The cached entity. */
  @volatile
  private var cachedEntity: Option[SoftReference[T]] = None

  /** The titles of this page. */
  final val titles: IO[Vector[Name]] =
    IO(cachedTitles) flatMap (_ map IO.pure getOrElse {
      for {
        _ <- IO.shift(environment.blocking)
        result <- IO(Source.fromURL(resource)(Codec.UTF8)).bracket { source =>
          IO(source.getLines.takeWhile(_ startsWith "#").map(_ substring 1).flatMap(Name(_)).toVector)
        }(s => IO(s.close()))
        _ <- IO.shift(environment.compute)
      } yield {
        cachedTitles = Some(result)
        result
      }
    })

  /** The main title of this page. */
  final val title: IO[Option[Name]] =
    titles map (_.headOption)

  /**
   * Attempts to load this page's document.
   *
   * @return The loaded document if available.
   */
  final val document: IO[Markup.Document] =
    IO(cachedDocument) flatMap (_ flatMap (_.get) map IO.pure getOrElse {
      for {
        raw <- new Parser(environment, assetTypes) parse resource
        resolved <- new Resolver(site, this) resolve raw
      } yield {
        cachedDocument = Some(SoftReference(resolved))
        resolved
      }
    })

  /** The description of this page. */
  final val description: IO[Vector[Markup.Inline]] =
    document map (_.descriptions)

  /**
   * Attempts to decode this page's entity.
   *
   * @return The decoded entity if available.
   */
  final val entity: IO[T] =
    IO(cachedEntity) flatMap (_ flatMap (_.get) map IO.pure getOrElse {
      for (entity <- document flatMap (scope.decoder.decode(_)(this))) yield {
        cachedEntity = Some(SoftReference(entity))
        entity
      }
    })

  /**
   * Attempts to publish this page's entity.
   *
   * @return The published entity if available.
   */
  final val publish: IO[String] =
    entity flatMap (scope.publisher.publish(_)(this))

  /** The environment that contains this page. */
  def environment: Environment

  /** The authors registered with the model. */
  def authors: Authors

  /** The types of assets registered with the model. */
  def assetTypes: Asset.Types

  /** The scope defined for this page. */
  def scope: Scope[T]

  /** The name of this page if it has one. */
  def name: Option[Name]

  /** The resource that describes this page. */
  def resource: URL

  /** The parent page if one exists. */
  def parent: Option[Page.Parent[_ <: AnyRef]]

  /** The nested children of this page. */
  def children: IO[Vector[Page.Child[_ <: AnyRef]]]

  /** The index of all pages in the containing site. */
  def index: IO[Index]

  /* Provide the stylesheets from this page and its parents. */
  override def stylesheets: IO[Vector[Styles]] =
    parent map (_.stylesheets map (_ ++ scope.stylesheets)) getOrElse IO.pure(scope.stylesheets)

  /* Resolve the specified author reference. */
  override def resolve(author: Author): IO[Option[Author]] =
    IO.pure(authors(author.name))

  /* Resolve the specified asset reference. */
  override def resolve[U <: Asset.Type](asset: Asset[U]): IO[Option[Asset.Resolved[U]]] =
    resolveAsset(asset) map (_ map { resolved =>
      finish(resolved.location) map (Asset.Relative(_, resolved.fileName, resolved.assetType)) getOrElse resolved
    })

  /* Resolve the specified entity reference. */
  override def resolve[U <: AnyRef : ClassTag](entity: Entity[U]): IO[Option[Entity.Resolved[U]]] =
    resolveEntity(entity) map (_ map { resolved =>
      finish(resolved.location) map Entity.Relative[U] getOrElse Entity.Absolute[U](resolved.location)
    })

  /* Load the specified entity reference. */
  override def load[U <: AnyRef : ClassTag](entity: Entity[U]): IO[Option[U]] =
    resolveEntity(entity) flatMap (_ map (p => p.entity map (Some(_))) getOrElse IO.pure(None))

  /**
   * Resolves assets into absolute assets by searching recursively up the site.
   *
   * @tparam U The type of asset to resolve.
   * @param asset The asset to resolve.
   * @return The result of attempting to resolve the asset.
   */
  private def resolveAsset[U <: Asset.Type](asset: Asset[U]): IO[Option[Asset.Absolute[U]]] = {

    /* Search the specified location for each file in turn until one is found. */
    def search(prefix: Location, remaining: Vector[String]): IO[Option[Asset.Absolute[U]]] =
      remaining match {
        case head +: tail =>
          resolveAsset[U](Asset.Absolute(prefix, head, asset.assetType)) flatMap {
            case None => search(prefix, tail)
            case some => IO.pure(some)
          }
        case _ => IO.pure(None)
      }

    asset match {
      case Asset.Named(assetName, assetType) =>
        Name(assetType.prefix) flatMap (n => Location(location.path :+ n)) map {
          search(_, assetType.extensions.toVector map (assetName + "." + _))
        } getOrElse IO.pure(None) flatMap {
          case None => parent map (p => (p: Page[_]).resolveAsset[U](asset)) getOrElse IO.pure(None)
          case some => IO.pure(some)
        }
      case Asset.Relative(path, fileName, tpe) =>
        Location(location.path ++ path) map { l =>
          resolveAsset[U](Asset.Absolute(l, fileName, tpe))
        } getOrElse IO.pure(None)
      case Asset.Absolute(location, fileName, tpe) =>
        environment.find(s"${location.path}/$fileName") map
          (_ map (_ => Asset.Absolute(location, fileName, tpe)))
    }
  }

  /**
   * Resolves entities into absolute entities by searching the site index when necessary.
   *
   * @tparam U The type of entity to resolve.
   * @param entity The entity to resolve.
   * @return The result of attempting to resolve the entity.
   */
  private def resolveEntity[U <: AnyRef : ClassTag](entity: Entity[U]): IO[Option[Page[U]]] = entity match {
    case Entity.Named(name) =>
      index map (_ (name, entity.assignableTo).sortBy(p => location.distanceTo(p.location)).headOption map
        (_.asInstanceOf[Page[U]]))
    case Entity.Relative(path) =>
      Location(location.path ++ path) map (l => resolveEntity[U](Entity.Absolute(l))) getOrElse IO.pure(None)
    case entity@Entity.Absolute(loc) =>
      index map (_ (loc, entity.assignableTo) map (_.asInstanceOf[Page[U]]))
  }

  /**
   * Finishes a resolved pointer by switching to relative paths that have the same or less number of tokens.
   *
   * @param result The absolute path to analyze.
   * @return The more appropriate relative path if one exists.
   */
  private def finish(result: Location): Option[Path] = {
    val common = location.commonPrefixWith(result)
    val prefix = location.path.elements.length - common.elements.length
    if (prefix + (result.path.elements.length - common.elements.length) <= result.path.elements.length)
      Some(Path(Vector.fill(prefix)(Path.Parent) ++ result.path.elements.drop(common.elements.length)))
    else None
  }

}

/**
 * Definitions of the supported page types.
 */
object Page {

  /**
   * Base type for pages that can have children.
   *
   * @tparam T The type of entity this page represents.
   */
  sealed trait Parent[T <: AnyRef] extends Page[T] {

    /** The cached children. */
    @volatile
    private var cachedChildren: Option[Vector[Child[_ <: AnyRef]]] = None

    /* Collect the children. */
    final override val children: IO[Vector[Child[_ <: AnyRef]]] = {

      def search(found: IO[Vector[Child[_ <: AnyRef]]], remaining: Vector[String]): IO[Vector[Child[_ <: AnyRef]]] =
        remaining match {
          case head +: tail => found flatMap { previous =>
            search((head match {
              case path if path endsWith ".md" => environment.find(path) map { document =>
                for {
                  doc <- document
                  name <- Name(path.substring(path.lastIndexOf('/') + 1, path.length - 3))
                } yield {
                  val s = scope(name)
                  Leaf(s.decoder.rename(name), s, doc, this)
                }
              }
              case path if path endsWith "/" => environment.find(s"$path/index.md") map { document =>
                for {
                  doc <- document
                  name <- Name(path.substring(path.lastIndexOf('/', path.length - 2) + 1, path.length - 1))
                } yield {
                  val s = scope(name)
                  Branch(s.decoder.rename(name), s, doc, this)
                }
              }
              case _ => IO.pure(None)
            }) map (_ map (previous :+ _) getOrElse previous), tail)
          }
          case _ => found
        }

      IO(cachedChildren) flatMap (_ map IO.pure getOrElse {
        environment.list(location.toString) flatMap (search(IO.pure(Vector.empty), _)) map { c =>
          cachedChildren = Some(c); c
        }
      })
    }

  }

  /**
   * Base type for pages that have parents.
   *
   * @tparam T The type of entity this page represents.
   */
  sealed trait Child[T <: AnyRef] extends Page[T] {

    /* Use the child name. */
    final override lazy val name: Option[Name] = Some(childName)

    /* Resolve against the parent's location. */
    final override lazy val location: Location = parentPage.location :+ childName

    /* Use the parent page. */
    final override lazy val parent: Option[Parent[_ <: AnyRef]] = Some(parentPage)

    /* Use the parent site. */
    final override def site: Site = parentPage.site

    /* Use the parent's environment. */
    final override def environment: Environment = parentPage.environment

    /* Use the parent's authors. */
    final override def authors: Authors = parentPage.authors

    /* Use the parent's asset types. */
    final override def assetTypes: Asset.Types = parentPage.assetTypes

    /* Use the parent's index. */
    final override def index: IO[Index] = parentPage.index

    /* Use the parent's support. */
    final override def alt(image: Asset.Resolved[Asset.Image]): IO[Option[String]] =
      parentPage.alt(image match {
        case relative@Asset.Relative(path, _, _) => relative.copy(path = childName +: path)
        case absolute => absolute
      })

    /** The parent of this page. */
    def parentPage: Parent[_ <: AnyRef]

    /** The name of this child. */
    def childName: Name

  }

  /**
   * The root node of a site.
   *
   * @tparam T The type of entity this page represents.
   * @param site        The site that contains this page.
   * @param environment The environment to operate in.
   * @param authors     The authors registered with the model.
   * @param assetTypes  The types of assets registered with the model.
   * @param scope       The root scope of the site.
   * @param resource    The document that describes this page.
   */
  case class Root[T <: AnyRef](
    site: Site,
    environment: Environment,
    authors: Authors,
    assetTypes: Asset.Types,
    scope: Scope[T],
    resource: URL
  ) extends Parent[T] {

    /** The cached index. */
    @volatile
    private var cachedIndex: Option[Index] = None

    /** The cached alt text. */
    private val cachedAltText = new ConcurrentHashMap[Location, Map[String, String]]()

    /* Lazily generate the index. */
    override val index: IO[Index] =
      IO(cachedIndex) flatMap (_ map IO.pure getOrElse (Index(this) map { i => cachedIndex = Some(i); i }))

    /* The root has no name. */
    override def name: Option[Name] = None

    /* The root has no location. */
    override def location: Location = Location.empty

    /* The root has no parent. */
    override def parent: Option[Parent[_ <: AnyRef]] = None

    /* Load the cached alt text. */
    override def alt(image: Asset.Resolved[Image]): IO[Option[String]] = {

      /* Recursively search for the specified name by removing extensions one at a time. */
      @annotation.tailrec
      def find(entries: Map[String, String], name: String): Option[String] = entries get name match {
        case None => name lastIndexOf '.' match {
          case i if i >= 0 => find(entries, name.substring(0, i))
          case _ => None
        }
        case some => some
      }

      (image match {
        case Asset.Relative(path, _, _) => Location(path)
        case Asset.Absolute(location, _, _) => Some(location)
      }) map { location =>
        Option(cachedAltText get location) map IO.pure getOrElse {
          val props = new Properties
          for {
            resource <- environment.find(s"${location}alt.properties")
            data <- IO(resource map (_.openStream)).bracket { stream =>
              IO(stream foreach props.load) map (_ => props.asScala.toMap)
            }(s => IO(s foreach (_.close())))
          } yield Option(cachedAltText.putIfAbsent(location, data)) getOrElse data
        } map (e => if (e.isEmpty) None else find(e, image.fileName))
      } getOrElse IO.pure(None)
    }

  }

  /**
   * A child node in a site that can contain children.
   *
   * @tparam T The type of entity this page represents.
   * @param childName  The name of this child.
   * @param scope      The scope of this branch.
   * @param resource   The document that describes this page.
   * @param parentPage The parent of this page.
   */
  case class Branch[T <: AnyRef](
    childName: Name,
    scope: Scope[T],
    resource: URL,
    parentPage: Parent[_ <: AnyRef]
  ) extends Parent[T] with Child[T]


  /**
   * A child node in a site that cannot contain children.
   *
   * @tparam T The type of entity this page represents.
   * @param childName  The name of this child.
   * @param scope      The scope of this branch.
   * @param resource   The document that describes this page.
   * @param parentPage The parent of this page.
   */
  case class Leaf[T <: AnyRef](
    childName: Name,
    scope: Scope[T],
    resource: URL,
    parentPage: Parent[_ <: AnyRef]
  ) extends Child[T] {

    /* Leaves have no children. */
    override val children: IO[Vector[Child[_ <: AnyRef]]] = IO.pure(Vector.empty)

  }

}
