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
trait ImageGeometry extends Parameter {

  /**
   * optionally define an offset
   */
  def offset(x: Int, y: Int) = new ImageGeometry {
    def spec =
      ImageGeometry.this.spec +
        (if (x >= 0) "+" + x else x.toString) +
        (if (y >= 0) "+" + y else y.toString)
    
    override def escape = ImageGeometry.this.escape
  }

  def apply(x: Int, y: Int) = offset(x, y)
}

trait ImageGeometryDefinition {
  def apply(pixel: Int) = new ImageGeometry {

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
    def area = new ImageGeometry {
      def spec = pixel + "@"
      override def escape = true
    }
  }

  def apply(width: Option[Int] = None, height: Option[Int] = None) = {
    (width, height) match {
      case (None, None) => error("either width or height must be defined at least")
      case (Some(w), Some(h)) => new ImageGeometry {
        def spec = w + "x" + h
      }
      case (Some(w), _) => new ImageGeometry {
        def spec = w.toString
      }
      case (_, Some(h)) => new ImageGeometry {
        def spec = "x" + h
      }
    }
  }

  /**
   * Maximum values of height and width given, aspect ratio preserved.
   */
  def apply(width: Int, height: Int) = new ImageGeometry {
    def spec = width + "x" + height

    /**
     * Minimum values of width and height given, aspect ratio preserved.
     */
    def min = new ImageGeometry {
      def spec = width + "x" + height + "^"
      override def escape = true
    }

    /**
     * Minimum values of width and height given, aspect ratio preserved.
     */
    def ^ = min

    /**
     * Width and height emphatically given, original aspect ratio ignored.
     */
    def force = new ImageGeometry {
      def spec = width + "x" + height + "!"
      override def escape = true
    }

    /**
     * Width and height emphatically given, original aspect ratio ignored.
     */
    def ! = force

    /**
     * Change as per widthxheight but only if an image dimension exceeds a specified dimension.
     */
    def ifGreater = new ImageGeometry {
      def spec = width + "x" + height + ">"
      override def escape = true
    }

    /**
     * Change as per widthxheight but only if an image dimension exceeds a specified dimension.
     */
    def > = ifGreater

    /**
     * Change dimensions only if both image dimensions exceed specified dimensions.
     */
    def ifBothGreater = new ImageGeometry {
      def spec = width + "x" + height + "<"
      override def escape = true
    }

    /**
     * Change dimensions only if both image dimensions exceed specified dimensions.
     */
    def < = ifBothGreater
  }

  def apply(scale: Double) = new ImageGeometry {
    assert(scale >= 0, "scale must be higher than 0")

    def spec = scale + "%"
    override def escape = true
  }

  def apply(scalex: Double, scaley: Double) = new ImageGeometry {
    assert(scalex >= 0, "scalex must be higher than 0")
    assert(scaley >= 0, "scaley must be higher than 0")

    def spec = scalex + "x" + scaley + "%"
    override def escape = true
  }
}

object Geometry extends ImageGeometryDefinition

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