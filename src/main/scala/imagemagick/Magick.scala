package org.imagemagick

import java.io.{IOException, File}
import scala.collection.JavaConversions._
import org.apache.commons.io.IOUtils

/**
 * User: mathias
 * Date: 01.03.11 20:23
 * Time: 20:23
 */
object ImageMagick {

  def convert(attributes: MagickAttributes): Magick = new Magick("convert", attributes)
  
  def convert: Magick = convert(MagickAttributes.empty)
}

class Magick(command: String, attributes: MagickAttributes = MagickAttributes.empty)
  extends Disposable
  with MagickAttributesSpecification
  with ColorSpecification {

  private[this] lazy val pb = new ProcessBuilder(command)

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

  private[this] def execute(prepared: PreparedMagick, file: File, outputAttributes: MagickAttributes = MagickAttributes.empty): MagickResult = {

    def files(files: Iterable[File]): Iterable[String] = files.map(_.getAbsolutePath)

    val commands = (command +:
      attributes.commands.toList ::
      prepared.commands ::
      outputAttributes.commands ::
      List(file.getAbsolutePath) :: Nil).flatten

    println(commands.mkString(" "))
    
    val process = new ProcessBuilder(commands).start
    val result = process.waitFor
    println("result code: " + result)
    println(IOUtils.readLines(process.getErrorStream))

    // TODO: magick result
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
trait PreparedMagick extends ImageManipulation with ImageGeometryDefinition {

  /**
   * the files to read from
   */
  def files: Iterable[File]

  /**
   * operations to apply
   */
  def operations: Iterable[Operation]

  def commands: Iterable[String] = {
    (files.map(_.getAbsolutePath) ::
      (operations.map(_.commands.toList).flatten) ::
      Nil).flatten
  }


  /**
   * write the result to the given file
   */
  def write(file: File, attributes: MagickAttributes = MagickAttributes.empty) = executor(this, file, attributes)

  protected[this] def executor: ((PreparedMagick, File, MagickAttributes) => MagickResult)
}

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
                              executor: ((PreparedMagick, File, MagickAttributes) => MagickResult),
                              operations: Iterable[Operation] = Nil)
  extends PreparedMagick {

  def apply(operation: Operation, moreOperations: Operation*): PreparedMagick = {
    val v = operations ++ List(operation) ++ Option(moreOperations).map(_.toList).getOrElse(Nil)
    SomePreparedMagick(files, executor, v)
  }
}

trait MagickAttributesSpecification {
  def size(width: Int, height: Int): MagickAttributes = new MagickAttributes{}.size(width, height)

  def quality(q: Int): MagickAttributes = new MagickAttributes{}.quality(q)
}

/**
 * define image attributes
 */
trait MagickAttributes extends HasCommands {
  def commands = attributes.map(_.commands).flatten

  def size(width: Int, height: Int) = apply(ParameterAttribute("-size", width + "x" + height))

  def quality(q: Int) = apply(ParameterAttribute("-quality", q.toString))

  def attributes: Iterable[MagickAttribute] = Nil

  def apply(attribute: MagickAttribute): MagickAttributes = {
    new MagickAttributes {
      override def attributes = MagickAttributes.this.attributes ++ List(attribute)
    }
  }
}

case class ParameterAttribute(command: String, parameter: () => Iterable[String]) extends MagickAttribute {
  def commands = Stream.cons(command, parameter().toStream)
}

object ParameterAttribute {
  def apply(command: String, parameter: => String): MagickAttribute = new MagickAttribute {
    def commands = List(command, parameter)
  }
}

trait MagickAttribute extends HasCommands

object MagickAttributes {
  def empty: MagickAttributes = new MagickAttributes {}
}

trait HasCommands {
  def commands: Iterable[String]
}

/**
 * contains the result of the imagemagick operation
 */
trait MagickResult
