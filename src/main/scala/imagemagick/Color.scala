package org.imagemagick

import scala.math.{min, max}

/**
 * User: mathias
 * Date: 02.03.11 22:12
 * Time: 22:12
 */

trait ColorSpecification {
  /**
   * predefined colors
   */
  lazy val black = Color("#000000")
  lazy val white = Color("#FFFFFF")
  lazy val red = Color("#FF0000")
  lazy val green = Color("#00FF00")
  lazy val blue = Color("#0000FF")
  lazy val none = Color("#00000000")
  lazy val opaque = black
  lazy val transparent = none

  def color(name: String) = Color(name)
  def color(red: Int, green: Int, blue: Int) = Color(red, green, blue)
}

object Color extends ColorSpecification {

  def apply(s: String): Color = new Color {
    lazy val spec = if (s.startsWith("#")) format("%s", s) else s
  }

  implicit def boxedValue(value: Int) = new {
    def boxed(lower: Int, upper: Int): Int = max(lower, min(upper, value))
  }

  def apply(red: Int, green: Int, blue: Int) = new Color {
    lazy val spec = format("rgb(%d,%d,%d)", red boxed (0, 255), green boxed (0, 255), blue boxed (0, 255))
  }
}

trait Color extends Parameter