package org.imagemagick

import java.io.{IOException, File}
import scala.collection.JavaConversions._
import org.apache.commons.io.IOUtils

/**
 * User: mathias
 * Date: 01.03.11 20:23
 * Time: 20:23
 */

object Magick {

  implicit def toSize(width: Int) = new {
    def x(height: Int) = new Size(width, height)
  }
}

trait Disposable {
  /**
   * clean up resources used by imagemagick
   */
  def dispose = {
    // TODO: cleanup used magick resources
  }
}

case class Magick(inputAttributes: ImageAttributes = ImageAttributes()) extends Disposable {

  private[this] val pb = new ProcessBuilder("convert")

  {
    // check that convert is available
    // TODO: move this code into some validate function with a responsive result to make it possible to react and to determine which version of imagemagick is installed
    val ret = try {
      pb.command(pb.command :+ "-version").start.waitFor
    } catch {
      case ex: IOException => {
        ex.printStackTrace
        -1
      }
    }
    assert(ret == 0, "imagemagick is not installed")
  }

  private[this] def execute(prepared: PreparedMagick, file: File, outputAttributes: ImageAttributes): MagickResult = {

    def files(files: Iterable[File]): Iterable[String] = files.map(_.getAbsolutePath)

    val commands = ("convert" +:
      inputAttributes.commands ::
      files(prepared.files) ::
      (prepared.operations.map(_.commands).flatten) ::
      outputAttributes.commands ::
      files(List(file)) ::
      Nil).flatten

    println(commands.mkString(" "))
    val process = new ProcessBuilder(commands).start
    val result = process.waitFor
    println("result code: " + result)
    println(IOUtils.readLines(process.getErrorStream))
    new MagickResult {}
  }

  def read(file: File, moreFiles: File*): PreparedMagick = {
    assert(file != null, "at least one file must be given")
    val files = file +: Option(moreFiles).map(_.toList).getOrElse(Nil)
    SomePreparedMagick(files, execute)
  }
}


/**
 * contains the result of the imagemagick operation
 */
trait MagickResult

/**
 * immutable instance to process image file operations
 */
trait PreparedMagick extends Disposable {

  /**
   * the files to read from
   */
  def files: Iterable[File]

  /**
   * operations to apply
   */
  def operations: Iterable[Operation]

  /**
   * add an operation to apply and create a new instance
   */
  def apply(operation: Operation, moreOperations: Operation*): PreparedMagick

  /**
   * crop out an area from image
   */
  def crop(area: Area) = apply(Crop(area))

  /**
   * create thumbnail
   */
  def thumbnail(size: Size) = apply(Thumbnail(size))

  /**
   * write the result to the given file
   */
  def write(file: File, attributes: ImageAttributes = ImageAttributes()) = executor(this, file, attributes)

  protected[this] def executor: ((PreparedMagick, File, ImageAttributes) => MagickResult)
}

/**
 * some operation which can be applyed to an image
 */
trait Operation extends HasCommands

/**
 * changes the size of an image to the given dimensions and removes any associated profiles.
 */
case class Thumbnail(size: Size) extends Operation {
  def commands = "-thumbnail" :: size.width + "x" + size.height :: Nil
}

/**
 * defines a crop operation
 */
case class Crop(area: Area) extends Operation {
  def commands = "-crop" :: area.size.width + "x" + area.size.height + "+" + area.x + "+" + area.y :: Nil
}

case class SomePreparedMagick(files: Iterable[File],
                              executor: ((PreparedMagick, File, ImageAttributes) => MagickResult),
                              operations: List[Operation] = Nil)
  extends PreparedMagick {

  def apply(operation: Operation, moreOperations: Operation*) = {
    val newOperations = (operations :+ operation) ::: Option(moreOperations).map(_.toList).getOrElse(Nil)
    SomePreparedMagick(files, executor, newOperations)
  }
}

/**
 * define an area
 */
case class Area(size: Size, x: Int = 0, y: Int = 0)

/**
 * defines width and height
 */
case class Size(width: Int, height: Int) {
  def +(x: Int) = new {
    def +(y: Int) = Area(Size.this, x, y)
  }
}

/**
 * scale image to desired size
 */
case class Scale(size: Size)

/**
 * define image attributes
 */
case class ImageAttributes(size: Option[Size] = None,
                           quality: Option[Int] = None) extends HasCommands {
  def commands = {
    val cmds = (for (s <- size) yield (List("-size", s.width + "x" + s.height))) ::
      (for (q <- quality) yield (List("-quality", q + ""))) :: Nil
    cmds.flatten.flatten
  }
}

trait HasCommands {
  def commands: List[String]
}