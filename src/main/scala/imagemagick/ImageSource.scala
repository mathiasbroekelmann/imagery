package org.imagemagick

import org.apache.commons.io.IOUtils
import java.io.{File, OutputStream, InputStream}

/**
 * identifies a command sequence with a image source
 */
trait HasSource extends ImageSettings with ImageCommands {

  self =>

  type Settings = HasSource

  /**
   * the image settings defined for this source
   */
  def settings: Iterable[HasCommands]

  /**
   * the defined source
   */
  def source: ImageSource

  def commands = settings ++ List(new HasCommands {
    def commands = source.sourceSpec :: Nil
  })

  /**
   * apply an image setting after the source
   */
  def apply(setting: ImageSetting) = new HasSource {

    def settings = self.settings

    override def commands = self.commands ++ (setting :: Nil)

    def source = self.source
  }
}

/**
 * User: mathias
 * Date: 05.03.11 14:33
 * Time: 14:33
 */
trait ImageSource extends HasSource {

  self =>

  /**
   * Plain source specification as it is used in the settings line
   */
  def sourceSpec: String

  /**
   * Write the data as input to the image magic settings via STDIN
   * By default this function does nothing.
   */
  def writeTo(out: OutputStream) = {}

  /**
   * faster and less resource intensive crop image as it is read
   */
  def cropped(geometry: ImageGeometry) =
    apply(List(new HasCommands {
      def commands = sourceSpec + "[" + geometry.spec + "]" :: Nil
    }))

  /**
   * faster and less resource intensive resize image as it is read
   */
  def resized(width: Int, height: Int) = cropped(Geometry(width, height))

  def apply(otherSettings: Iterable[HasCommands] = Nil): HasSource = new HasSource {

    override def commands: Iterable[HasCommands] = self.commands ++ otherSettings

    def source = self

    def settings = self.settings
  }

}

trait ImageSourceSpec extends Commands {

  /**
   * Create an image source for a built in pattern.
   */
  def pattern(pattern: String) = new {

    /**
     * define the size for the pattern
     */
    def size(width: Int, height: Int, offset: Option[Int] = None) =
      PatternInputSource(commands, pattern).size(width, height, offset)
  }

  /**
   * Create an image stream source for the given input stream.
   * Use InputStreamSource#buffered to use the stream source multiple times
   */
  def image(in: InputStream) = InputStreamSource(commands, in)

  /**
   * create an image source for a given file
   */
  def image(file: File) = FileInputSource(commands, file)

  /**
   * create an image source for a given file
   */
  def image(file: String): FileInputSource = image(new File(file))
}

object ImageSourceSpec extends ImageSourceSpec {
  def commands = Nil
}

case class PatternInputSource(settings: Iterable[HasCommands], name: String) extends ImageSource {
  def sourceSpec = "pattern:" + name

  def source = this
}

trait StreamSource extends ImageSource {
  def sourceSpec = "-"
}

case class InputStreamSource(settings: Iterable[HasCommands], input: InputStream) extends StreamSource with ImageSource {

  /**
   * Create a buffered stream source which can be used multiple times to read image data from
   * Calling this method immediatly reads the data of the stream and stores it in memory
   */
  def buffered = ByteArrayInputSource(commands, IOUtils.toByteArray(input))

  override def writeTo(output: OutputStream) = {
    assert(input.available > 0, "input stream is either already read or is empty. " +
      "If the first is the case use #buffered to be able to read the data twice")
    try {
      IOUtils.copy(input, output)
    } finally {
      input.close
    }
  }

  def source = this
}

case class ByteArrayInputSource(settings: Iterable[HasCommands], bytes: Array[Byte]) extends StreamSource with ImageSource {

  override def writeTo(output: OutputStream) = {
    IOUtils.write(bytes, output)
  }

  def source = this
}

case class FileInputSource(settings: Iterable[HasCommands], file: File) extends ImageSource {

  self =>

  def source = self

  def sourceSpec = file.getAbsolutePath

  /**
   * define frames of the images to use use <code>frames(0 to 3)</code>
   */
  def frames(range: scala.Range) = new ImageSource {
    nested =>

    def sourceSpec = {
      if (range.step == 1) {
        file.getAbsolutePath + "[" + range.start + "-" + range.end + "]"
      } else {
        file.getAbsolutePath + range.mkString("[", ",", "]")
      }
    }

    def source = nested

    def settings = self.settings
  }

  /**
   * define frames of the images to use use <code>frames(3,2,4)</code>
   */
  def frames(index: Int, moreIndexes: Int*) = new ImageSource {
    nested =>

    val indexes: Iterable[Int] = index :: Option(moreIndexes).getOrElse(Nil).toList

    def sourceSpec = {
      file.getAbsolutePath + indexes.mkString("[", ",", "]")
    }

    def source = nested

    def settings = self.settings
  }

}

