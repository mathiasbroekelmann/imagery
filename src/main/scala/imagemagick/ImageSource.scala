package org.imagemagick

import org.apache.commons.io.IOUtils
import java.io.{File, OutputStream, InputStream}

/**
 * User: mathias
 * Date: 05.03.11 14:33
 * Time: 14:33
 */
trait ImageSource extends HasCommands with ImageAttributes {

  def imageSource = this

  def commands = attributes.map(_.commands).flatten ++ sourceCommands

  def sourceSpec: String

  def sourceCommands = sourceSpec :: Nil

  /**
   * Write the data as input to the image magic command via STDIN
   * By default this function does nothing.
   */
  def writeTo(out: OutputStream) = {}

  /**
   * faster and less resource intensive crop image as it is read
   */
  def cropped(geometry: ImageGeometry) = new ImageSource {

    override def attributes = ImageSource.this.attributes

    def sourceSpec = ImageSource.this.sourceSpec + "[" + geometry.spec + "]"
  }

  /**
   * faster and less resource intensive resize image as it is read
   */
  def resized(width: Int, height: Int) = cropped(Geometry(width, height))
}

trait ImageSourceSpec {

  /**
   * Create an image source for a built in pattern.
   */
  def pattern(pattern: String) = new {
    
    /**
     * define the size for the pattern
     */
    def size(width: Int, height: Int, offset: Option[Int] = None) = PatternInputSource(pattern).size(width, height, offset)
  }

  /**
   * Create an image stream source for the given input stream.
   * Use InputStreamSource#buffered to use the stream source multiple times
   */
  def image(in: InputStream) = InputStreamSource(in)

  /**
   * create an image source for a given file
   */
  def image(file: File) = FileInputSource(file)
}

object ImageSourceSpec extends ImageSourceSpec

case class PatternInputSource(name: String) extends ImageSource {
  def sourceSpec = "pattern:" + name
}

trait StreamSource extends ImageSource {
  def sourceSpec = "-"
}

case class InputStreamSource(input: InputStream) extends StreamSource {

  /**
   * Create a buffered stream source which can be used multiple times to read image data from
   * Calling this method immediatly reads the data of the stream and stores it in memory
   */
  def buffered = ByteArrayInputSource(IOUtils.toByteArray(input))

  override def writeTo(output: OutputStream) = {
    assert(input.available > 0, "input stream is either already read or is empty. " +
      "If the first is the case use #buffered to be able to read the data twice")
    try {
      IOUtils.copy(input, output)
    } finally {
      input.close
    }
  }
}

case class ByteArrayInputSource(bytes: Array[Byte]) extends StreamSource {
  override def writeTo(output: OutputStream) = {
    IOUtils.write(bytes, output)
  }
}

case class FileInputSource(file: File) extends ImageSource {
  def sourceSpec = file.getAbsolutePath

  /**
   * define frames of the images to use use <code>frames(0 to 3)</code>
   */
  def frames(range: scala.Range) = new ImageSource {
    def sourceSpec = {
      if (range.step == 1) {
        file.getAbsolutePath + "[" + range.start + "-" + range.end + "]"
      } else {
        file.getAbsolutePath + range.mkString("[", ",", "]")
      }
    }
  }

  /**
   * define frames of the images to use use <code>frames(3,2,4)</code>
   */
  def frames(index: Int, moreIndexes: Int*) = new ImageSource {
    val indexes: Iterable[Int] = index :: Option(moreIndexes).getOrElse(Nil).toList

    def sourceSpec = {
      file.getAbsolutePath + indexes.mkString("[", ",", "]")
    }
  }
}

