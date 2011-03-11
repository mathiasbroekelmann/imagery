package org.imagemagick

import collection.JavaConversions._
import java.io.{InputStream, OutputStream}
import org.apache.commons.io.IOUtils

/**
 * User: mathias
 * Date: 07.03.11 20:32
 * Time: 20:32
 */
trait RequiresImageSource extends ImageSettings with ImageSourceSpec {

  /**
   * since an image source is required we stay here until we have an image source
   */
  type Settings = RequiresImageSource

  /**
   * After getting a source we allow definition of commands
   */
  type HasSource = HasImageSource

  def commands: Iterable[HasCommands]

  def apply(setting: ImageSetting) = SomeRequiresImageSource(commands ++ (setting :: Nil))

  def apply(image: ImageSource) = ConvertCommand(commands ++ (image :: Nil))
}

/**
 * defines the convert command which allows writing to some output image
 */
trait Convert extends HasImageSource with ImageWriter with Sugar {

  /**
   * result of executions will be this type
   */
  type ExecutionResult = Convert

  def executionResult = this

  def command = "convert"
}

/**
 * mixin trait for executions
 */
trait Execution {

  /**
   * define the execution result
   */
  type ExecutionResult

  def command: String

  def executionResult: ExecutionResult

  /**
   * executes the commands defined in this image writer with additional arguments supplied to this function
   */

  def execute(arguments: Iterable[String],
              in: Option[InputStream] = None,
              out: Option[OutputStream] = None): ExecutionResult = {
    val commands = command :: arguments.toList
    val process: Process = new ProcessBuilder(commands).start
    for(input <- in) {
      IOUtils.copy(input, process.getOutputStream)
    }
    val result = process.waitFor
    if(result != 0) {
      throw new RuntimeException("Result code " + result + " by executing: " + commands)
    }
    for(output <- out) {
      IOUtils.copy(process.getInputStream, output)
    }
    executionResult
  }
}

case class ConvertCommand(commands: Iterable[HasCommands]) extends Convert {

  def apply(setup: HasCommands) = ConvertCommand(commands ++ (setup :: Nil))
}

/**
 * combines different image magic command sequences into a single operation.
 */
trait Sugar extends HasImageSource {

  type HasSource = HasImageSource

  /**
   * Surround the image with a border of color.
   */
  def border(geometry: ImageGeometry, color: Color): Operators = borderColor(color).border(geometry)
}

object Convert extends RequiresImageSource {
  def commands = Nil

  def convert = Convert
}

case class SomeRequiresImageSource(commands: Iterable[HasCommands]) extends RequiresImageSource