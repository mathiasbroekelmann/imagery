package org.mbr.imagery.resources

import java.net.URI
import org.mbr.imagery.image.Image
import javax.activation.MimeType
import java.io.{FilenameFilter, FileFilter, File}
import java.lang.String
import org.mbr.imagery.blob.{FileBlob, UriBlob}
import javax.servlet.ServletContext

/**
 * User: mathias
 * Date: 27.02.11 16:26
 * Time: 16:26
 */

case class HtmlImage(loc: URI, relativeTo: URI) extends Image {

  def blob = new UriBlob {
    override lazy val uri = loc
  }

  def src = URI.create(loc.toString.stripPrefix(relativeTo.toString))
}

trait Dashboard {

  def location: File

  /**
   * get the latest images sorted by their last modified date.
   */
  lazy val latest: Iterable[HtmlImage] = Nil

  /**
   * get all pictures below location
   */
  lazy val pictures: Iterable[HtmlImage] = {
    location match {
      case dir if dir.isDirectory => images(dir)
      case file => for (img <- image(file)) yield img
    }
  }

  /**
   * ignore hidden directories starting with .
   */
  def acceptDirectory(dir: File): Boolean = !dir.getName.startsWith(".")

  implicit def richFile(file: File) = new {
    def pathRelativeTo(ancestor: File): String = {
      file.getAbsolutePath.stripPrefix(ancestor.getAbsolutePath)
    }
  }

  /**
   * create an image instance for the given file if it is a valid image
   */
  private def image(file: File): Option[HtmlImage] = {
    val mimeType = Option(file.toURI.toURL.openConnection.getContentType).map(new MimeType(_))
    mimeType match {
      case Some(m) if "image" == m.getPrimaryType => Some(HtmlImage(file.toURI, location.toURI))
      case _ => None
    }
  }

  private def images(directory: File): Iterable[HtmlImage] = {

    def file(name: String): File = new File(directory, name)

    def imageFiles: Iterable[File] = Option(directory.list(new FilenameFilter() {
      def accept(file: File) = file.isFile && file.canRead

      def accept(dir: File, name: String) = accept(new File(dir, name))
    })).map(_.toStream).getOrElse(Stream.empty).map(file)

    def nestedDirectories: Iterable[File] = Option(directory.list(new FilenameFilter() {
      def accept(file: File) = file.isDirectory && file.canRead && acceptDirectory(file)

      def accept(dir: File, name: String) = accept(new File(dir, name))
    })).map(_.toStream).getOrElse(Stream.empty).map(file)

    val pictures = for (file <- imageFiles; image <- image(file)) yield image
    val nested = for (dir <- nestedDirectories) yield images(dir)

    pictures ++ nested.flatten
  }
}