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
  def strip = apply(Strip)

  def define(settings: Map[String, String]) = apply(Define(settings))

  /**
   * create thumbnail
   */
  def thumbnail(size: Size) = apply(Thumbnail(size))

  /**
   * write the result to the given file
   */
  def write(file: File, attributes: Iterable[Attribute] = Nil): MagickResult
}

case class Define(settings: Map[String, String]) extends Operation {
  def commands = "-define" :: settings.toList.map(e => "%s=%s".format(e._1, e._2))
}

object Strip extends Operation {
  def commands = "-strip" :: Nil
}

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


/**
 * contains the result of the imagemagick operation
 */


/**
 * immutable instance to process image file operations
 */


/**
 * some operation which can be applyed to an image
 */


/**
 * changes the size of an image to the given dimensions and removes any associated profiles.
 */


/**
 * defines a crop operation
 */


/**
 * define an area
 */


/**
 * defines width and height
 */


/**
 * scale image to desired size
 */


/**
 * define image attributes
 */


