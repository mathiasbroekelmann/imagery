package org.imagemagick

import org.apache.commons.io.output.ByteArrayOutputStream
import org.apache.commons.io.IOUtils
import java.io.{FileOutputStream, InputStream, File, OutputStream}
import scala.util.logging.Logged

/**
 * defines the function to create result images
 */
trait ImageWriter extends Commands with Execution with Logged {

  /**
   * define the format of the image to write. A format must be defined if image format can not
   * be determined through the location or filename
   */
  def writeAs(format: String) = write.as(format)

  /**
   * write the image to the given file
   */
  def writeTo(location: String) = write.to(location)

  /**
   * write the image to the given file
   */
  def writeTo(file: File) = write.to(file)

  /**
   * write images by using the provied image writer
   */
  def write: WriteImage[ExecutionResult] = {

    def executeWithInputStream[A](execute: (Iterable[String], Option[OutputStream => Unit]) => A): A = {
      // traverse commands
      // if streamsource found and use it as stdin
      // write all remaining input sources to a temp file and use that file as file reference

      /**
       * process the remaining commands without an inputstream before.
       * arguments of traversed commands are provided for execution
       */
      def withoutStream(remainingCommands: Iterable[HasCommands], arguments: Iterable[String]): A = {
        remainingCommands.headOption match {
          case None => execute(arguments, None)
          case Some(stream: StreamSource) => withStream(remainingCommands.tail, arguments ++ stream.commands, stream.writeTo(_))
          case Some(other) => withoutStream(remainingCommands.tail, arguments ++ other.commands)
        }
      }

      /**
       * process the remaining commands where an inputstream was defined before.
       * arguments and the defined inputstream of traversed commands are provided for execution
       */
      def withStream(remainingCommands: Iterable[HasCommands],
                     arguments: Iterable[String],
                     stdin: OutputStream => Unit): A = {

        /**
         * pipe the stream content to a file
         */
        def pipedToFile(stream: StreamSource): File = {
          val file = File.createTempFile("magick_", "")
          val out = new FileOutputStream(file)
          try {
            log("piping stream source %s to %s".format(stream, file))
            stream writeTo out
          } finally {
            out.close
          }
          file
        }

        /**
         * pipe stream to file and inject the location path as source spec to the stream commands
         */
        def piped(stream: StreamSource) = {
          val file = pipedToFile(stream)
          try {
            val args = stream.commands(file.getAbsolutePath)
            withStream(remainingCommands.tail, arguments ++ args, stdin)
          } finally {
            file.delete
          }
        }

        remainingCommands.headOption match {
          case None => execute(arguments, Some(stdin))
          case Some(stream: StreamSource) => piped(stream)
          case Some(other) => withStream(remainingCommands.tail, arguments ++ other.commands, stdin)
        }
      }

      withoutStream(commands, Nil)
    }

    new WriteOperation(
      (writeArgs, out) =>
        executeWithInputStream {
          (commandArgs, input) =>
            execute(commandArgs ++ writeArgs, input, out)
        }
    )
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
class WriteOperation[A](exec: (Iterable[String], Option[InputStream => Unit]) => A,
                        format: Option[String] = None)
  extends WriteImage[A] {

  def to(file: File) = to(file.getAbsolutePath)

  def to(location: String) = {
    val args = format match {
      case Some(format) if format.trim.size > 0 => (format + ":" + location) :: Nil
      case _ => location :: Nil
    }
    exec(args, None)
  }

  def as(format: String) = {
    require(format.trim.size > 0, "invalid format definition: " + format)
    new WriteOperation(exec, Some(format)) with WriteImageWithFormat[A] {

      def bytes = {
        val out = new ByteArrayOutputStream
        to(out)
        out.toByteArray
      }

      def to(out: OutputStream) = {

        def read(in: InputStream) {
          try {
            IOUtils.copy(in, out)
          } finally {
            out.close
          }
        }

        exec(format + ":-" :: Nil, Some(read))
      }
    }
  }
}