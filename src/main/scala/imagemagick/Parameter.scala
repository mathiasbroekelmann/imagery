package org.imagemagick

/**
 * User: mathias
 * Date: 06.03.11 20:43
 * Time: 20:43
 */

/**
 * defines a parameter which may be applied to an operation or setting
 */
trait Parameter {
  def spec: String
  def escape: Boolean = false
}

case class ParameterOperation(operation: String, param: Parameter) extends Operation {
  def commands = operation :: param.spec :: Nil
}

object ParameterOperation {
  def apply(operation: String, value: Any) = new Operation {
    def commands = operation :: value.toString :: Nil
  }
}

/**
 * mixin trait for parameters which may be flagged with ^, !, < or >
 */
trait Flagable[A] {

  def build(flag: String, escape: Boolean = false): A

  /**
   * Minimum values of width and height given, aspect ratio preserved.
   */
  def min = build("^", true)

  /**
   * Minimum values of width and height given, aspect ratio preserved.
   */
  def ^ = min

  /**
   * Width and height emphatically given, original aspect ratio ignored.
   */
  def force = build("!", true)

  /**
   * Width and height emphatically given, original aspect ratio ignored.
   */
  def ! = force

  /**
   * Change as per widthxheight but only if an image dimension exceeds a specified dimension.
   */
  def ifGreater = build(">", true)

  /**
   * Change as per widthxheight but only if an image dimension exceeds a specified dimension.
   */
  def > = ifGreater

  /**
   * Change dimensions only if both image dimensions exceed specified dimensions.
   */
  def ifBothGreater = build("<", true)

  /**
   * Change dimensions only if both image dimensions exceed specified dimensions.
   */
  def < = ifBothGreater
}