package org.imagemagick

import collection.JavaConversions._
import java.io.{InputStream, OutputStream}
import org.apache.commons.io.IOUtils
import util.logging.{Logged, ConsoleLogger}

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
}

/**
 * defines the convert command which allows writing to some output image
 */
trait Convert extends HasImageSource with Sugar {

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
trait Execution extends Logged {

  /**
   * define the execution result
   */
  type ExecutionResult

  /**
   * define the command location to execute
   */
  def command: String

  /**
   * define the execution result
   */
  def executionResult: ExecutionResult

  /**
   * Executes the given arguments against command
   *
   * TODO: settle a timeout for executions to avoid block issues
   *
   * @param arguments provide the arguments to execute
   * @param in Some function which receives an outputstream to write to the outputstream is piped to STDIN. None if no such data is defined
   * @param out Some outputstream which is filled with STDOUT or None
   * @param err Some outputstream which is filled with STDERR or None
   */
  def execute(arguments: Iterable[String],
              in: Option[OutputStream => Unit] = None,
              out: Option[InputStream => Unit] = None,
              err: Option[InputStream => Unit] = None): ExecutionResult = {
    val commands = command :: arguments.toList
    log("executing " + commands.mkString(" "))
    val process: Process = new ProcessBuilder(commands).start
    val outStream = process.getOutputStream
    try {
      for (input <- in) {
        input(outStream)
      }
    } finally {
      outStream.close
    }
    val result = process.waitFor
    val errStream = process.getErrorStream
    try {
      for (someErr <- err) {
        someErr(errStream)
      }
    } finally {
      errStream.close
    }

    if (result != 0) {
      val msg = err match {
        case None => IOUtils.readLines(process.getErrorStream)
        case _ => "details where provided as error stream"
      }
      throw new RuntimeException("Result code " + result + " by executing: " + commands + ": " + msg)
    }
    val inStream = process.getInputStream
    try {
      for (output <- out) {
        output(inStream)
      }
    } finally {
      inStream.close
    }
    executionResult
  }
}

/**
 * combines different image magic command sequences into a single operation.
 */
trait Sugar extends HasImageSource {

  /**
   * Surround the image with a border of color.
   */
  def border(geometry: ImageGeometry, color: Color) = borderColor(color).border(geometry)
}

/**
 * convert command which requires an input source
 */
class ConvertRequiresImageSource(val commands: Iterable[HasCommands]) extends RequiresImageSource with Logged {
  self =>

  def apply(setting: ImageSetting) = new ConvertRequiresImageSource(commands ++ (setting :: Nil)) {
    override def log(msg: String) = self.log(msg)
  }

  def apply(image: => ImageSource) = new ConvertWithInputSource(commands ++ (image :: Nil)) {
    override def log(msg: String) = self.log(msg)
  }
}

/**
 * convert command with an input source
 */
class ConvertWithInputSource(val commands: Iterable[HasCommands]) extends Convert {
  self =>

  def apply(setup: HasCommands) = new ConvertWithInputSource(commands ++ (setup :: Nil)) {
    override def log(msg: String) = self.log(msg)
  }

  type HasSource = Convert
}


object Convert extends ConvertRequiresImageSource(Nil) with ConsoleLogger {
  def convert = Convert
}