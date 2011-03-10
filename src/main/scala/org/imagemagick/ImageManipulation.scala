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
trait ImageManipulation extends MagickOperations with ColorSpecification {

  /**
   * define additional parameters. see -define option
   */
  def define(settings: Map[String, String]) = apply(Define(settings))

  /**
   * crop out an area from image
   */
  def crop(geometry: ImageGeometry) = apply(ParameterOperation("-crop", geometry))

  /**
   * strip all profiles from image
   */
  def strip = apply(SimpleOperation("-strip"))

  /**
   * Automagically orient (rotate) an image created by a digital camera.
   */
  def autoorient = apply(SimpleOperation("-auto-orient"))

  /**
   * sharpen the image with an unsharp mask operator.
   *
   * @param radius    The radius of the Gaussian, in pixels,  not counting the center
   *                  pixel (default 0).
   * @param sigma     The standard deviation of the Gaussian, in pixels (default 1.0).
   * @param amount    The fraction of the difference between the original and the blur
   *                  image that is added back into the original (default 1.0).
   * @param threshold The threshold, as a fraction of QuantumRange, needed to apply the
   *                  difference amount (default 0.05).
   */
  def unsharp(radius: Int, sigma: Double = 1.0, amount: Double = 1.0, threshold: Double = 0.05) =
    apply(new Operation {
      def commands = "-unsharp" :: radius + "x" + sigma + "+" + amount + "+" + threshold :: Nil
    })

  /**
   * changes the size of an image to the given dimensions and removes any associated profiles.
   */
  def thumbnail(geometry: ImageGeometry) = apply(ParameterOperation("-thumbnail", geometry))

  /**
   * Set the image size and offset.
   */
  def extent(geometry: ImageGeometry) = apply(ParameterOperation("-extent", geometry))

  /**
   * 
   */
  def background(color: Color = white) = apply(ParameterOperation("-background", color))

  def background(color: String) = apply(ParameterOperation("-background", Color(color)))
  /**
   * Sets the current gravity suggestion for various other settings and options.
   */
  def gravity(gravity: Gravity.Gravity = Gravity.NorthWest) = apply(ParameterOperation("-gravity", gravity))

  /**
   * Adjust the canvas and offset information of the image.
   */
  def repage(geometry: ImageGeometry) = apply(ParameterOperation("-repage", geometry))

  /**
   * completely remove/reset the virtual canvas meta-data from the images
   */
  def repage = apply(SimpleOperation("+repage"))

  /**
   * write the result to the given file
   */
  def write(file: File, attributes: MagickAttributes = MagickAttributes.empty): MagickResult
}

object Gravity extends Enumeration {
  type Gravity = Value
  val None, Center, East, Forget, NorthEast, North, NorthWest, SouthEast, South, SouthWest, West, Static = Value
}

case class Define(settings: Map[String, String]) extends Operation {
  def commands = "-define" :: Nil

  //settings.toList.map(e => "%s=%s".format(e._1, e._2))
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
case class Size(width: Int, height: Int, offset: Option[Int] = None) extends AreaDefinition {

  def x(xOffset: Int) = Area(width, height, xOffset, 0)

  def y(yOffset: Int) = Area(width, height, 0, yOffset)

  override def toString = width + "x" + height + offset.map("+" + _).getOrElse("")
}

/**
 * scale image to desired size
 */
case class Scale(size: Size)

case class SimpleOperation(command: String) extends Operation {
  def commands = List(command)
}

