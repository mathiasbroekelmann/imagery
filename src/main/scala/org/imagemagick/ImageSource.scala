package org.imagemagick

import org.apache.commons.io.IOUtils
import java.io.{File, OutputStream, InputStream}

/**
 * identifies a command sequence with a image source
 */
trait HasImageSource extends ImageSettings with ImageOperators with ImageSourceSpec with ImageWriter {

  type Settings = HasSource

  type Operators = HasSource

  def apply(setup: HasCommands): HasSource

  def apply(image: => ImageSource) = apply(image.asInstanceOf[HasCommands])

  def apply(setting: ImageSetting) = apply(setting.asInstanceOf[HasCommands])

  def apply(operator: ImageOperator) = apply(operator.asInstanceOf[HasCommands])
}

/**
 * Defines an image source
 */
trait ImageSource extends HasCommands {

  self =>

  /**
   * Plain source specification as it is used in the settings line
   */
  def sourceSpec: String

  def commands: Iterable[String] = commands(sourceSpec)

  def commands(sourceSpec: String): Iterable[String] = sourceSpec :: Nil

  /**
   * Write the data as input to the image magic settings via STDIN
   * By default this function does nothing.
   */
  def writeTo(out: OutputStream) = {}

  /**
   * faster and less resource intensive crop image as it is read
   */
  def cropped(geometry: ImageGeometry) =
    ParameterizedImageSource(self, geometry = Option(geometry))

  /**
   * faster and less resource intensive resize image as it is read
   */
  final def resized(width: Int, height: Int) =
    cropped(Geometry(width, height))

  /**
   * define frames of the images to use use <code>frames(0 to 3)</code>
   */
  final def frames(range: scala.Range): ParameterizedImageSource =
    frames(RangeFrames(range))

  /**
   * define frames of the images to use use <code>frames(3,2,4)</code>
   */
  def frames(index: Int, moreIndexes: Int*): ParameterizedImageSource = {
    val indexes = index :: Option(moreIndexes).getOrElse(Nil).toList
    frames(CollectionFrames(indexes))
  }

  def frames(frames: Frames) = ParameterizedImageSource(self, frames = Option(frames))
}

/**
 * an image source that is parameterized
 */
case class ParameterizedImageSource(origin: ImageSource,
                                    geometry: Option[ImageGeometry] = None,
                                    frames: Option[Frames] = None) extends ImageSource {

  def sourceSpec = origin.sourceSpec + (geometry :: frames :: Nil).flatten.map(_.spec).mkString("[", "", "]")

  override def frames(frames: Frames) = ParameterizedImageSource(origin, geometry, Option(frames))

  override def cropped(geometry: ImageGeometry) = ParameterizedImageSource(origin, Option(geometry), frames)

  override def writeTo(out: OutputStream) = origin.writeTo(out)

  override def commands(sourceSpec: String) = origin.commands(sourceSpec)
}

/**
 * a frames defintion with explicit frame indexes
 */
case class CollectionFrames(indexes: Iterable[Int]) extends Frames {
  def spec = indexes.mkString(",")
}

/**
 * a frames definition with a frame range
 */
case class RangeFrames(range: scala.Range) extends Frames {

  def spec = {
    if (range.step == 1) {
      range.start + "-" + range.end
    } else {
      range.mkString(",")
    }
  }
}

/**
 * contract for frames
 */
trait Frames extends Parameter {
}

/**
 * mixin trait which allows the definition of image sources
 */
trait ImageSourceSpec {

  /**
   * this type will be available after an image has been defined
   */
  type HasSource <: HasImageSource

  /**
   * apply the image setting the the list of existing commands.
   */
  def apply(image: => ImageSource): HasSource

  /**
   * Create an image source for a built in pattern.
   */
  def pattern(pattern: String) = new {

    /**
     * define the size for the pattern
     */
    def size(width: Int,
             height: Int,
             offset: Option[Int] = None) =
      PatternInputSource(pattern, width, height, offset)
  }

  /**
   * Create an image stream source for the given input stream.
   * Use InputStreamSource#buffered to use the stream source multiple times
   */
  implicit def image(in: InputStream) =
    InputStreamSource(in)

  /**
   * create an image source for a given file
   */
  implicit def image(file: File) =
    FileInputSource(file)

  /**
   * create an image source for a given file
   */
  implicit def image(file: String) =
    FileInputSource(new File(file))
}

/**
 * defines an input source for a pattern
 */
case class PatternInputSource(name: String,
                              width: Int,
                              height: Int,
                              offset: Option[Int] = None) extends ImageSource {
  def sourceSpec = "pattern:" + name

  /**
   * we need to prepend the size definition before the pattern spec
   */
  override def commands(sourceSpec: String) = {
    ImageSettings.size(width, height, offset)
      .commands
      .map(_.commands)
      .flatten ++ (sourceSpec :: Nil)
  }

}

/**
 * mixin trait for stream based image sources
 */
trait StreamSource extends ImageSource {
  def sourceSpec = "-"
}

/**
 * a stream based image source
 */
case class InputStreamSource(input: InputStream) extends StreamSource with ImageSource {

  /**
   * Create a buffered stream source which can be used multiple times to read image data from
   * Calling this method immediatly reads the data of the stream and stores it in memory
   */
  def buffered = {
    val bytes = try {
      IOUtils.toByteArray(input)
    } finally {
      input.close
    }
    ByteArrayInputSource(bytes)
  }

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

case class ByteArrayInputSource(bytes: Array[Byte]) extends StreamSource with ImageSource {

  override def writeTo(output: OutputStream) = {
    IOUtils.write(bytes, output)
  }
}

case class FileInputSource(file: File) extends ImageSource {

  def sourceSpec = file.getAbsolutePath
}

