package org.mbr.imagery.image

import java.net.URI
import javax.activation.MimeType
import java.io.{FilenameFilter, File}
import org.mbr.imagery.blob.FileBlob
import javax.ws.rs.core.UriBuilder

/**
 * User: mathias
 * Date: 22.03.11 21:53
 * Time: 21:53
 */

/**
 * an image store provides access to images which are based by some location
 */
trait ImageStore {

  self =>

  type Store <: ImageStore

  type Image <: StoredImage

  /**
   * define the base location of the images
   */
  def location: File

  /**
   * @return the uri of this store below the root store
   */
  def storeUri: URI

  /**
   * @return Some parent if there is one or None if not.
   */
  def parent: Option[Store];

  /**
   * @return the name of the image store
   */
  def name: String = location.getName

  protected def nestedImageStore(name: String): Store

  protected def storedFileImage(file: File, storeUri: URI): Image

  /**
   * return nested child image stores
   */
  def childStores: Iterable[Store] = {

    Option(location.list(new FilenameFilter() {
        def accept(file: File) = acceptDirectory(file)

        def accept(dir: File, name: String) = accept(new File(dir, name))
      })).map(_.toStream).getOrElse(Stream.empty).map(nestedImageStore(_))
  }

  /**
   * @return Some child store if it is a valid child otherwise None
   */
  def childStore(name: String): Option[Store] = {
    val directory = new File(location, name)
    if(acceptDirectory(directory)) {
      Some(nestedImageStore(name))
    } else {
      None
    }
  }

  /**
   * ignore hidden directories starting with.
   */
  def acceptDirectory(dir: File): Boolean = dir.canRead && dir.isDirectory && !dir.getName.startsWith(".")

  /**
   * loads an image for the given store uri.
   *
   * @return Some image if the uri identifies a valid image. Otherwise None
   */
  def load(imageUri: URI): Option[Image] = {
    val file = new File(location, imageUri.getSchemeSpecificPart)
    if(file.canRead) {
      image(file)
    } else {
      None
    }
  }

  /**
   * get all pictures below location
   */
  def pictures: Iterable[Image] = {

    def loadimages(directory: File): Iterable[Image] = {

      def file(name: String): File = new File(directory, name)

      def imageFiles: Iterable[File] = Option(directory.list(new FilenameFilter() {
        def accept(file: File) = file.isFile && file.canRead

        def accept(dir: File, name: String) = accept(new File(dir, name))
      })).map(_.toStream).getOrElse(Stream.empty).map(file)

      def nestedDirectories: Iterable[File] = Option(directory.list(new FilenameFilter() {
        def accept(file: File) = file.isDirectory && file.canRead && acceptDirectory(file)

        def accept(dir: File, name: String) = accept(new File(dir, name))
      })).map(_.toStream).getOrElse(Stream.empty).map(file)

      for (file <- imageFiles.toStream; image <- image(file)) yield image
    }

    location match {
      case dir if dir.isDirectory => loadimages(dir)
      case file => for (img <- image(file)) yield img
    }
  }

  /**
   * create an image instance for the given file if it is a valid image
   */
  private[this] def image(file: File): Option[Image] = {

    lazy val storeUri = URI.create(file.toURI.toString.stripPrefix(location.toURI.toString))

    val mimeType = Option(file.toURI.toURL.openConnection.getContentType).map(new MimeType(_))
    mimeType match {
      case Some(m) if "image" == m.getPrimaryType => Some(storedFileImage(file, storeUri))
      case _ => None
    }
  }

}

/**
 * defines an image which is stored
 */
trait StoredImage extends Image {

  /**
   * @return the uri of the image in the store.
   */
  def storeUri: URI
}

case class StoredFileImage(file: File, storeUri: URI) extends StoredImage with FileBlob

case class LocationImageStore(location: String)

case class NestedImageStore(someParent: ImageStore, directoryName: String) extends ImageStore {
  self =>

  type Store = ImageStore

  type Image = StoredImage

  lazy val parent = Some(someParent)

  lazy val storeUri = UriBuilder.fromUri(someParent.storeUri).path(directoryName).build()

  lazy val location = new File(someParent.location, directoryName)

  protected def nestedImageStore(name: String) = NestedImageStore(self, name)

  protected def storedFileImage(file: File, storeUri: URI) = StoredFileImage(file, storeUri)
}