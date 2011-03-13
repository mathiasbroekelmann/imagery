package org.imagemagick

/**
 * User: mathias
 * Date: 07.03.11 20:32
 * Time: 20:32
 */

/**
 * An image operator differs from a setting in that it affects the image immediately as it appears on the command line
 */
trait ImageOperators extends Commands {

  /**
   * abstract result type after an operator was applied
   */
  type Operators <: ImageOperators

  /**
   * apply the image operator the the list of existing operators.
   * function is used to create the result of the apply function.
   */
  def apply(operator: ImageOperator): Operators

  /**
   * Annotate an image with text.
   */
  def annotate(degrees: Int, text: String) =
    apply(new ParameterImageOperator("annotate", degrees.toString :: text :: Nil))

  /**
   * Annotate an image with text.
   */
  def annotate(xdegrees: Int, ydegrees: Int, text: String) =
    apply(new ParameterImageOperator("annotate", (xdegrees + "x" + ydegrees) :: text :: Nil))

  /**
   * Annotate an image with text.
   */
  def annotate(xdegrees: Int, ydegrees: Int, tx: Int, ty: Int, text: String) = {

    /**
     * add the -/+ prefix to the given value in depend of its value
     */
    def arg(value: Int) = (if (value >= 0) "+" else "") + value

    /**
     * param computation
     */
    def param = (xdegrees + "x" + ydegrees + arg(tx) + arg(ty)) :: text :: Nil

    apply(new ParameterImageOperator("annotate", param))
  }

  /**
   * Automagically orient (rotate) an image created by a digital camera.
   */
  def autoOrient = apply(new ParameterImageOperator("auto-orient"))

  /**
   * Force to black all pixels below the threshold given as an absolute integer while leaving all pixels at or above the threshold unchanged.
   */
  def blackThreshold(value: Int) = {
    require(value >= 0, "threshold must not be lower than 0")
    apply(new ParameterImageOperator("black-threshold", value.toString))
  }

  /**
   * Force to black all pixels below the threshold given as an percent value while leaving all pixels at or above the threshold unchanged.
   */
  def blackThreshold(percent: Double) = {
    require(percent >= 0 && percent <= 100, "percent must not be lower than 0 or greater than 100")
    apply(new ParameterImageOperator("black-threshold", percent + "%"))
  }

  /**
   * Reduce image noise and reduce detail levels.
   */
  def blur(radius: Int) = apply(new ParameterImageOperator("blur", radius.toString))

  /**
   * Reduce image noise and reduce detail levels.
   */
  def blur(radius: Int, sigma: Int) = apply(new ParameterImageOperator("blur", radius + "x" + sigma))

  /**
   * Surround the image with a border of color.
   */
  def border(geometry: ImageGeometry) = apply(new ParameterImageOperator("border", geometry.spec))

  /**
   * Simulate a charcoal drawing.
   */
  def charcoal(factor: Int) = apply(new ParameterImageOperator("charcoal", factor.toString))

  /**
   * Remove pixels from the interior of an image.
   */
  def chop(geometry: ImageGeometry) = apply(new ParameterImageOperator("chop", geometry.spec))

  /**
   * Set the preferred number of colors in the image.
   */
  def colors(value: Int) = apply(new ParameterImageOperator("colors", value.toString))

  /**
   * Colorize the image by an amount specified by value using the color specified by the most recent fill setting.
   */
  def colorize(value: Int) = apply(new ParameterImageOperator("colorize", value.toString))

  /**
   * Colorize the image by an amount specified by value using the color specified by the most recent fill setting.
   */
  def colorize(red: Int = 0, green: Int = 0, blue: Int = 0) =
    apply(new ParameterImageOperator("colorize", (red :: green :: blue :: Nil).mkString(",")))

  /**
   * Set the image colorspace.
   */
  def colorspace(value: Colorspace.Colorspace) =
    apply(new ParameterImageOperator("colorspace", value.toString))

  /**
   * enhance the image contrast
   */
  def enhanceContrast = apply(new ParameterImageOperator("contrast", Nil))

  /**
   * reduce the image contrast
   */
  def reduceContrast = apply(new ParameterImageOperator("contrast", Nil, "+"))

  //TODO: convolve

  /**
   * Cut out one or more rectangular regions of the image.
   */
  def crop(geometry: ImageGeometry) = apply(new ParameterImageOperator("crop", geometry.spec))

  /**
   * Create a thumbnail of the image.
   */
  def thumbnail(geometry: ImageGeometry) = apply(new ParameterImageOperator("thumbnail", geometry.spec))

  /**
   * Reduce image noise and reduce detail levels.
   */
  def unsharp(radius: Int) = apply(new ParameterImageOperator("unsharp", radius.toString))

  /**
   * Reduce image noise and reduce detail levels.
   */
  def unsharp(radius: Int, sigma: Double, threshold: Option[Int] = None) =
    apply(new ParameterImageOperator("unsharp", radius + "x" + sigma + threshold.map("+" + _).getOrElse("")))

  def extent(geometry: ImageGeometry) = apply(new ParameterImageOperator("extent", geometry.spec))
}

trait ImageOperator extends HasCommands

class ParameterImageOperator(name: String, param: => Iterable[String], flag: String = "-") extends ImageOperator {

  def this(name: String, param: String*) = this (name, Option(param).map(_.toIterable).getOrElse(Nil))

  def commands = flag + name :: param.toList
}

object Colorspace extends Enumeration {
  type Colorspace = Value

  val CMY, CMYK, Gray, HSB, HSL, HWB, Lab, Log, OHTA, Rec601Luma, Rec601YCbCr, Rec709Luma, Rec709YCbCr = Value
  val RGB, sRGB, Transparent, XYZ, YCbCr, YCC, YIQ, YPbPr, YUV = Value

}