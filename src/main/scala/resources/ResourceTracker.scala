package resources

import java.net.URI
import org.joda.time.DateTime
import java.io.{InputStream, OutputStream}

/**
 * User: mathias
 * Date: 24.03.11 22:10
 * Time: 22:10
 */

trait ResourceTracker {
}

trait ResourceRepository {
  /**
   * unique id which identifies this resource repository
   */
  def id: String

  def scan(existing: Iterable[Resources]): Iterable[ResourceEvent]
}

trait ResourceEvent

case class ResourceDeleted(resource: Resource) extends ResourceEvent
case class ResourceUpdated(resource: Resource) extends ResourceEvent
case class ResourceCreated(resource: Resource) extends ResourceEvent

/**
 * provides access to existing resources and their metadata
 */
trait Resources {
  def exists(resource: Resource)
}

trait Resource {

  /**
   * the uri of the resource
   */
  def uri: URI

  /**
   * the size of the resource
   */
  def size: Option[Int]

  /**
   * last modified time
   */
  def lastModified: Option[DateTime]

  /**
   * read data from the resource
   */
  def read[A](f: InputStream ⇒ A): Option[A]

  /**
   * @return Some resource content as bytes. may be None if resource is empty or not available.
   */
  def bytes: Option[Array[Byte]]

  /**
   * write the resource data to the given stream.
   */
  def write(out: OutputStream): Unit
}