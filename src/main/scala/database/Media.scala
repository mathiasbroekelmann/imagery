package org.mbr.imagery.database

import java.sql.Timestamp
import java.net.URI

import org.squeryl._
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.annotations._

import scala.collection._

import image.metadata.Orientation
import Orientation._

/**
 * Defines an image record
 */
class Image(
  val src: String,
  val size: Long,
  val lastModified: Timestamp,
  val width: Option[Int],
  val height: Option[Int],
  val orientation: Option[Orientation]) extends KeyedEntity[Int] {

  val id: Int = 0

  @Transient lazy val location: URI = URI.create(src)

  def this() = this(null, 0, new Timestamp(0), Some(0), Some(0), Some(TopLeft))

  def dimension = (width, height) match {
    case (Some(w), Some(h)) ⇒ Some((w, h))
    case _ ⇒ None
  }

  /**
   * @return all tags associated which this image
   */
  def tags = Media.imageTags.left(this)
}

/**
 * Tags may be used to link entities together
 *
 * @author mathias
 *
 */
case class Tag(
  val name: String)
  extends KeyedEntity[Int] {

  val id: Int = 0

  def parents = Media.nestedTags.left(this)

  def childs = Media.nestedTags.right(this)
}

/**
 * mixin trait for tag association types
 */
trait EntityTags
  extends KeyedEntity[CompositeKey2[Int, Int]] {

  def entityId: Int
  def tagId: Int

  def id = compositeKey(entityId, tagId)
}

/**
 * image tag associations
 */
case class ImageTags(val entityId: Int, val tagId: Int) extends EntityTags

/**
 * tat to tag associations
 */
case class NestedTags(val entityId: Int, val tagId: Int) extends EntityTags

object Media extends Media {

}

class Media extends Schema {

  val images = table[Image]

  val tags = table[Tag]

  on(images)(i ⇒ declare(
    i.id is (primaryKey, autoIncremented)))

  on(tags)(i ⇒ declare(
    i.name is (unique),
    i.id is (primaryKey, autoIncremented)))

  override def drop = {
    super.drop
  }

  // Relations
  /**
   * tagged images
   */
  val imageTags = manyToManyRelation(images, tags) via associatedTags[ImageTags]

  /**
   * nested tags
   */
  val nestedTags = manyToManyRelation(tags, tags) via associatedTags[NestedTags]

  /**
   * Return all images which where assigned to the given tags
   */
  def imagesByTags = taggedBy(imageTags)

  /**
   * helper functions for tagged entities
   */
  def associatedTags[A <: EntityTags](entity: KeyedEntity[Int], tag: Tag, association: A) =
    (association.entityId === entity.id, association.tagId === tag.id)

  def taggedBy[A <: KeyedEntity[Int], B <: Tag, C <: EntityTags](relation: ManyToManyRelation[A, B, C]) =
    new {
      /**
       * provide tags as a query so we can make a single database call
       */
      def apply(tags: Query[B]): Query[A] =
        from(relation.thisTable, tags, relation.leftTable)((relation, tag, entity) ⇒
          where(tag.id === relation.tagId and entity.id === relation.entityId)
            select (entity)) distinct

      /**
       * provide tags as a collection
       */
      def apply(tags: Iterable[B]): Query[A] =
        tags match {
          case query: Query[B] ⇒ apply(query)
          case other ⇒
            from(relation.thisTable, relation.leftTable)((relation, entity) ⇒
              where(relation.entityId === entity.id and (relation.tagId in (other.map(_.id))))
                select (entity)) distinct
        }

      def apply(tags: B*): Query[A] = apply(tags.toIterable)
    }
}