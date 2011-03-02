package org.imagemagick

import scala.math.{min, max}

/**
 * User: mathias
 * Date: 02.03.11 22:12
 * Time: 22:12
 */

object Color {
  def color(s: String): Color = new Color {
    val spec = if (s.startsWith("#")) format("'%s'", s) else s
  }

  /**
   * predefined colors
   */
  lazy val black = color("#000000")
  lazy val white = color("#FFFFFF")
  lazy val red = color("#FF0000")
  lazy val green = color("#00FF00")
  lazy val blue = color("#0000FF")
  lazy val none = color("#00000000")
  lazy val opaque = black
  lazy val transparent = none

  implicit def boxedValue(value: Int) = new {
    def boxed(lower: Int, upper: Int): Int = max(lower, min(upper, value))
  }

  def rgb(red: Int, green: Int, blue: Int) = new Color {

    val spec = format("rgb(%d,%d,%d)", red boxed (0, 255), green boxed (0, 255), blue boxed (0, 255))
  }
}

trait Color {
  def spec: String
}
