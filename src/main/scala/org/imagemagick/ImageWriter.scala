package org.imagemagick

import org.apache.commons.io.output.ByteArrayOutputStream
import java.io.{InputStream, File, OutputStream}

/**
 * defines the function to create result images
 */
trait ImageWriter extends Execution {

  /**
   * write images by using the provied image writer
   */
  def write: WriteImage[ExecutionResult] = {

    def exec(arguments: Iterable[String], out: Option[OutputStream]): ExecutionResult = {
      execute(arguments, None, out)
    }

    new WriteOperation(exec)
  }
}

/**
 * defines functions to write images
 */
trait WriteImage[A] {

  /**
   * define the format of the image to write. A format must be defined if image format can not
   * be determined through the location or filename
   */
  def as(format: String): WriteImageWithFormat[A]

  /**
   * write the image to the given file
   */
  def to(location: String): A

  /**
   * write the image to the given file
   */
  def to(file: File): A
}

/**
 * defines an image writer with an explicit image format.
 */
trait WriteImageWithFormat[A] extends WriteImage[A] {

  /**
   * writes the image data to the given output stream
   */
  def to(out: OutputStream): A

  /**
   * returns the image data as a byte array
   */
  def bytes: Array[Byte]
}

/**
 * Write images to various targets
 */
class WriteOperation[A](f: (Iterable[String], Option[OutputStream]) => A,
                        format: Option[String] = None)
  extends WriteImage[A] {

  def to(file: File) = to(file.getAbsolutePath)

  def to(location: String) = {
    val args = format match {
      case Some(format) if format.trim.size > 0 => (format + ":" + location) :: Nil
      case _ => location :: Nil
    }
    f(args, None)
  }

  def as(format: String) = {
    require(format.trim.size > 0, "invalid format definition: " + format)
    new WriteOperation(f, Some(format)) with WriteImageWithFormat[A] {

      def bytes = {
        val out = new ByteArrayOutputStream
        to(out)
        out.toByteArray
      }

      def to(out: OutputStream) = f(format + ":-" :: Nil, Some(out))
    }
  }
}