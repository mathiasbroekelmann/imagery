package org.mbr.imagery.blob

import java.io._
import javax.activation.MimeType

import org.apache.commons.io.IOUtils._
import java.lang.Math._
import java.net.{URLConnection, URI}

/**
 * a blob define some binary data together with access to its metadata.
 */
trait Blob {

  type Source = {
    def uri: URI
    def data: Option[InputStream]
    def size: Option[Long]
    def lastModified: Option[Long]
    def contentType: Option[MimeType]
  }

  /**
   * define the location data of the blob.
   */
  def source: Source

  def uri: URI = source.uri

  /**
   * @return Some size of the blob or None if the size is unknown.
   */
  def size = source.size

  /**
   * @return Some date when the blob was last modified as unix timestamp or None if it is not defined
   */
  def lastModified = source.lastModified

  /**
   * @return Some content type of the blob or None if it is unknown
   */
  def contentType = source.contentType

  /**
   * read data from the blob
   */
  def read[A](f: InputStream ⇒ A): Option[A] = {
    for (in ← source.data) yield {
      try {
        f(in)
      } finally {
        in.close
      }
    }
  }

  /**
   * @return the blob as bytes. may be empty if blob is empty or not available.
   */
  def bytes: Option[Array[Byte]] = {
    read(toByteArray(_))
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

/**
 * some file based blob
 */
trait FileBlob extends Blob {

  def file: File

  lazy val source = new {

    def uri = file.toURI

    def data = if (file.canRead) Some(new FileInputStream(file)) else None

    def size = if (file.canRead) Some(file.length) else None

    def lastModified = if (file.canRead) Some(file.lastModified) else None

    def contentType = {
      if (file.canRead)
        Option(file.toURI.toURL.openConnection.getContentType).map(new MimeType(_))
      else
        None
    }
  }
}

/**
 * some uri based blob.
 */
trait UriBlob extends Blob {

  self =>

  def proxy: Option[java.net.Proxy] = None

  def uri: URI

  lazy val source = new {

    def uri = self.uri

    private val con: URLConnection = proxy match {
      case Some(p) => uri.toURL.openConnection(p)
      case None => uri.toURL.openConnection
    }
    con.connect

    def data = Some(uri.toURL.openStream)

    val size = Some(con.getContentLength).map(_.asInstanceOf[Long])
    val lastModified = Option(con.getLastModified)
    val contentType = Option(con.getContentType).map(new MimeType(_))

    con.getInputStream.close
  }
}