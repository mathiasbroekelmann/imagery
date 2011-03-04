package org.imagemagick

/**
 * User: mathias
 * Date: 04.03.11 23:39
 * Time: 23:39
 */
trait BorderOperations extends MagickOperations {

  /**
   * Set the border color.
   *
   * The color is specified using the format described under the -fill option.
   * The default border color is #DFDFDF, this shade of gray.
   */
  def bordercolor(color: Color) = apply(ParameterOperation("-bordercolor", color))

  /**
   * Surround the image with a border of color.
   *
   * Set the width and height using the size portion of the gravity argument.
   * See Image Geometry for complete details about the geometry argument. Offsets are ignored.
   *
   * Set the border color by preceding with the bordercolor setting.
   * The default border color is #DFDFDF, this shade of gray.
   */
  def border(geometry: ImageGeometry) = apply(ParameterOperation("-border", geometry))

  /**
   * Set the border width.
   */
  def borderwidth(geometry: ImageGeometry) = apply(ParameterOperation("-borderwidth", geometry))
}
