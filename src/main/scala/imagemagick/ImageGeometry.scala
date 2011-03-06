package org.imagemagick

/**
 * Specify image geometry.
 *
 * @see http://www.imagemagick.org/script/command-line-processing.php#geometry
 *
 * User: mathias
 * Date: 04.03.11 21:48
 * Time: 21:48
 */
trait ImageGeometry extends Parameter

/**
 * identifies an image geometry with specified size
 */
trait SizeImageGeometry extends ImageGeometry {

  /**
   * define an offset for the size
   */
  def offset(x: Int, y: Int) = OffsetImageGeometry(this, x, y)

  /**
   * alternative to define an offset
   */
  def apply(x: Int, y: Int) = offset(x, y)
}

/**
 * identifies a size image geometry of width and height which was flaged
 */
case class FlagedImageGeometry(spec: String, override val escape: Boolean) extends SizeImageGeometry

/**
 * identifies an image geometry with an offset
 */
case class OffsetImageGeometry(size: SizeImageGeometry, x: Int, y: Int) extends ImageGeometry {
  def spec = size.spec +
    (if (x >= 0) "+" + x else x.toString) +
    (if (y >= 0) "+" + y else y.toString)

  override def escape = size.escape
}

/**
 * mixin trait for image geometry definitions
 */
trait ImageGeometryDefinition {
   def apply(pixel: Int) = new SizeImageGeometry {

    /**
     * treat pixel as width
     */
    def spec = pixel.toString

    /**
     * Resize image to have specified area in pixels. Aspect ratio is preserved.
     */
    def \@ = area

    /**
     * Resize image to have specified area in pixels. Aspect ratio is preserved.
     */
    def area = new SizeImageGeometry {
      def spec = pixel + "@"

      override def escape = true
    }
  }

  def apply(width: Option[Int] = None, height: Option[Int] = None): SizeImageGeometry = {
    (width, height) match {
      case (None, None) => error("either width or height must be defined at least")
      case (Some(w), Some(h)) => apply(w, h)
      case (Some(w), _) => new SizeImageGeometry {
        def spec = w.toString
      }
      case (_, Some(h)) => new SizeImageGeometry {
        def spec = "x" + h
      }
    }
  }

  /**
   * Maximum values of height and width given, aspect ratio preserved.
   */
  def apply(width: Int, height: Int) = new SizeImageGeometry with Flagable[SizeImageGeometry] {
    def spec = width + "x" + height
    def build(flag: String, esc: Boolean) =
      FlagedImageGeometry(spec + flag, escape || esc)
  }

  def apply(scale: Double) = new SizeImageGeometry {
    assert(scale >= 0, "scale must be higher than 0")

    def spec = scale + "%"

    override def escape = true
  }

  def apply(scalex: Double, scaley: Double) = new SizeImageGeometry {
    assert(scalex >= 0, "scalex must be higher than 0")
    assert(scaley >= 0, "scaley must be higher than 0")

    def spec = scalex + "x" + scaley + "%"

    override def escape = true
  }
}

/**
 * singleton for geometry definitions
 */
object Geometry extends ImageGeometryDefinition