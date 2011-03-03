package org.imagemagick

import java.io.{IOException, File}
import scala.collection.JavaConversions._
import org.apache.commons.io.IOUtils
import org.imagemagick._

/**
 * User: mathias
 * Date: 01.03.11 20:23
 * Time: 20:23
 */
trait ImageManipulation extends MagickOperations {

  /**
   * crop out an area from image
   */
  def crop(area: Area) = apply(Crop(area))

  /**
   * strip all profiles from image
   */
  def strip = apply(SimpleOperation("-strip"))

  /**
   * define additional parameters. see -define option
   */
  def define(settings: Map[String, String]) = apply(Define(settings))

  def autoorient = apply(SimpleOperation("-auto-orient"))

  def unsharp(settings: UnsharpSpec) = apply(Unsharp(settings))

  /**
   * create thumbnail
   */
  def thumbnail(size: Size) = apply(Thumbnail(size))

  /**
   * write the result to the given file
   */
  def write(file: File, attributes: Iterable[Attribute] = Nil): MagickResult
}

trait Parameters {
  def width(width: Int) = new {
    def height(height: Int) = Size(width, height)
  }
}

case class Unsharp(spec: UnsharpSpec) extends Operation {
  def commands = spec.commands
}

trait UnsharpSpec extends HasCommands

trait UnsharpDefinition {
  def radius(radius: Int) = new UnsharpSpec {
    def commands = format("%d", radius) :: Nil

    def x(sigma: Double) = new UnsharpSpec {
      def commands = format("%dx%s", radius, sigma) :: Nil

      def +(amount: Double) = new UnsharpSpec {
        def commands = format("%dx%s+%s", radius, sigma, amount) :: Nil

        def +(threshold: Double) = new UnsharpSpec {
          def commands = format("%dx%s+%s+%s", radius, sigma, amount, threshold) :: Nil
        }
      }
    }
  }
}

object UnsharpDefinition extends UnsharpDefinition

case class Define(settings: Map[String, String]) extends Operation {
  def commands = "-define" :: Nil //settings.toList.map(e => "%s=%s".format(e._1, e._2))
}

/**
 * changes the size of an image to the given dimensions and removes any associated profiles.
 */
case class Thumbnail(size: Size) extends Operation {
  def commands = "-thumbnail" :: size.toString :: Nil
}

/**
 * defines a crop operation
 */
case class Crop(area: Area) extends Operation {
  def commands = "-crop" :: area.toString :: Nil
}

trait AreaDefinition {

  def width: Int

  def height: Int

  def area = Area(width, height)
}

/**
 * define an area
 */
case class Area(width: Int, height: Int, x: Int = 0, y: Int = 0) extends AreaDefinition {
  def size = Size(width, height)


  def x(xOffset: Int): Area = Area(width, height, xOffset, y)

  def y(yOffset: Int): Area = Area(width, height, x, yOffset)

  override def toString =
    width + "x" + height +
      (if (x > 0) "+" + x else x) +
      (if (y > 0) "+" + y else y)
}

/**
 * defines width and height
 */
case class Size(width: Int, height: Int) extends AreaDefinition {

  def x(xOffset: Int) = Area(width, height, xOffset, 0)

  def y(yOffset: Int) = Area(width, height, 0, yOffset)

  override def toString = width + "x" + height
}

/**
 * scale image to desired size
 */
case class Scale(size: Size)

case class SimpleOperation(command: String) extends Operation {
  def commands = List(command)
}

