package org.mbr.imagery.blob

import java.io._
import javax.activation.MimeType

import org.apache.commons.io.IOUtils._
import java.lang.Math._
import java.net.{URLConnection, URI}

trait Blob {

  type Location = {
    def uri: URI
    def data: Option[InputStream]
    def size: Option[Long]
    def lastModified: Option[Long]
    def contentType: Option[MimeType]
  }

  def uri = location.uri

  def size = location.size

  def lastModified = location.lastModified

  def contentType = location.contentType

  /**
   * define the location of the blob data.
   */
  def location: Location

  /**
   * read data from the blob
   */
  def read[A](f: InputStream ⇒ A): Option[A] = {
    for (in ← location.data) yield {
      try {
        f(in)
      } finally {
        in.close
      }
    }
  }

  def available = location.data.isDefined

  /**
   * @return the blob as bytes. may be empty if blob is empty or not available.
   */
  def bytes: Array[Byte] = {
    read(toByteArray(_)).getOrElse(new Array[Byte](0))
  }

  /**
   * write the blob data to the given stream.
   *
   * @return number of bytes written to the stream. 0 if no bytes are written
   */
  def write(out: OutputStream) = {
    read(in ⇒
      try {
        copy(in, out)
      } finally {
        out.close
      }).map(max(_, 0)).getOrElse(0)
  }
}

trait FileBlob extends Blob {

  def file: File

  def location = new {
    def uri = file.toURI

    def data = if (file.canRead) Some(new FileInputStream(file)) else None

    def size = if (file.canRead) Some(file.length) else None

    def lastModified = if (file.canRead) Some(file.lastModified) else None

    def contentType = None
  }
}

trait UriBlob extends Blob {

  def proxy: Option[java.net.Proxy] = None

  def uri: URI

  lazy val location = new {

    private val con: URLConnection = proxy match {
      case Some(p) => uri.toURL.openConnection(p)
      case None => uri.toURL.openConnection
    }
    con.connect
    lazy val uri = UriBlob.this.uri

    def data = Some(uri.toURL.openStream)

    val size = Some(con.getContentLength).map(_.asInstanceOf[Long])
    val lastModified = Option(con.getLastModified)
    val contentType = Option(con.getContentType).map(new MimeType(_))

    con.getInputStream.close
  }
}