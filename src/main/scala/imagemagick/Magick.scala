package org.imagemagick

import java.io.{IOException, File}
import scala.collection.JavaConversions._
import org.apache.commons.io.IOUtils

/**
 * User: mathias
 * Date: 01.03.11 20:23
 * Time: 20:23
 */
object Magick extends Parameters with UnsharpDefinition {

  def convert(attributes: Attribute*): Magick =
    convert(Option(attributes).map(_.toList).getOrElse(Nil))

  def convert(attributes: Iterable[Attribute]): Magick =
    Magick(attributes)

  def read(file: File, moreFiles: File*) = new Magick().read(file, moreFiles: _*)

  val size = AttributeCommand[Size](s => "-size" :: s.width + "x" + s.height :: Nil)

  val quality = AttributeCommand[Int](q => "-quality" :: q.toString :: Nil)
}
case class AttributeCommand[A](c: A => Iterable[String]) {
  def ->(value: A) = new Attribute {
    def commands = c(value)
  }
}

case class Magick(attributes: Iterable[Attribute] = Nil) extends Disposable {

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

  private[this] def execute(prepared: PreparedMagick, file: File, outputAttributes: Iterable[Attribute]): MagickResult = {

    def files(files: Iterable[File]): Iterable[String] = files.map(_.getAbsolutePath)

    val commands = "convert" +:
      (attributes.map(_.commands.toList).flatten ::
        files(prepared.files) ::
        (prepared.operations.map(_.commands.toList).flatten) ::
        outputAttributes.map(_.commands.toList).flatten ::
        files(List(file)) ::
        Nil).flatten.toList

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

trait MagickOperations {

  /**
   * add one or more operations to apply and create a new instance
   */
  def apply(operation: Operation, moreOperation: Operation*): PreparedMagick
}

/**
 * immutable instance to process image file operations
 */
trait PreparedMagick extends ImageManipulation {

  /**
   * the files to read from
   */
  def files: Iterable[File]

  /**
   * operations to apply
   */
  def operations: Iterable[Operation]

  /**
   * write the result to the given file
   */
  def write(file: File, attributes: Iterable[Attribute] = Nil) = executor(this, file, attributes)

  protected[this] def executor: ((PreparedMagick, File, Iterable[Attribute]) => MagickResult)
}

trait Attribute extends HasCommands

trait Disposable {
  /**
   * clean up resources used by imagemagick
   */
  def dispose = {
    // TODO: cleanup used magick resources
  }
}

/**
 * some operation which can be applyed to an image
 */
trait Operation extends HasCommands

case class SomePreparedMagick(files: Iterable[File],
                              executor: ((PreparedMagick, File, Iterable[Attribute]) => MagickResult),
                              operations: Iterable[Operation] = Nil)
  extends PreparedMagick {

  def apply(operation: Operation, moreOperations: Operation*): PreparedMagick = {
    val v = operations ++ List(operation) ++ Option(moreOperations).map(_.toList).getOrElse(Nil)
    SomePreparedMagick(files, executor, v)
  }
}
/**
 * define image attributes
 */
case class ImageAttributes(size: Option[Size] = None,
                           quality: Option[Int] = None) extends HasCommands {
  def commands = {
    val cmds = (for (s <- size) yield ("-size" :: s.width + "x" + s.height :: Nil)) ::
      (for (q <- quality) yield ("-quality" :: q + "" :: Nil)) :: Nil
    cmds.flatten.flatten
  }
}

trait HasCommands {
  def commands: Iterable[String]
}

/**
 * contains the result of the imagemagick operation
 */
trait MagickResult
