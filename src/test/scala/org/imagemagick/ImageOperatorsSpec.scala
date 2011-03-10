package org.imagemagick

import org.specs.Specification
import java.io.File
import Convert._
import Color._

/**
 * User: mathias
 * Date: 10.03.11 21:05
 * Time: 21:05
 */

class ImageOperatorsSpec extends Specification {

  "image operators" should {

    val file = new File("src/test/resources/fruehling.jpg")
    val base = convert(image(file))

    "command sequence has a valid image source" in {
      base must notBeNull
    }

    import base._

    "annotate" in {

      "with degrees" in {
        val (path :: operation :: param :: text :: Nil) = annotate(180, "Hello World!").arguments.toList
        path must endWith("fruehling.jpg")
        operation must_== ("-annotate")
        param must_== ("180")
        text must_== ("Hello World!")
      }

      "with x/y degrees" in {
        val (path :: operation :: param :: text :: Nil) = annotate(180, 190, "Hello Scala!").arguments.toList
        path must endWith("fruehling.jpg")
        operation must_== ("-annotate")
        param must_== ("180x190")
        text must_== ("Hello Scala!")

        "with offset" in {
          val (path :: operation :: param :: text :: Nil) = annotate(180, 190, 30, -20, "Hello ImageMagick!").arguments.toList
          path must endWith("fruehling.jpg")
          operation must_== ("-annotate")
          param must_== ("180x190+30-20")
          text must_== ("Hello ImageMagick!")
        }
      }
    }

    "black threshold" in {

      "with absolute integer value" in {
        val (path :: operation :: param :: Nil) = blackThreshold(20).arguments.toList
        path must endWith("fruehling.jpg")
        operation must_== ("-black-threshold")
        param must_== ("20")

        "fail early if value is not lower than 0" in {
          blackThreshold(-1) must throwA[IllegalArgumentException]
        }
      }

      "with percent" in {
        val (path :: operation :: param :: Nil) = blackThreshold(20.0).arguments.toList
        path must endWith("fruehling.jpg")
        operation must_== ("-black-threshold")
        param must_== ("20.0%")

        "fail early if value is not in range" in {
          blackThreshold(-.1) must throwA[IllegalArgumentException]
          blackThreshold(100.1) must throwA[IllegalArgumentException]
        }
      }
    }

    "blur" in {

      "radius only" in {
        val (path :: operation :: param :: Nil) = blur(2).arguments.toList
        path must endWith("fruehling.jpg")
        operation must_== ("-blur")
        param must_== ("2")
      }

      "radius and sigma" in {
        val (path :: operation :: param :: Nil) = blur(radius = 0, sigma = 2).arguments.toList
        path must endWith("fruehling.jpg")
        operation must_== ("-blur")
        param must_== ("0x2")
      }
    }

    "border" in {
      val (path :: operation :: param :: Nil) = border(Geometry(100, 200)).arguments.toList
      path must endWith("fruehling.jpg")
      operation must_== ("-border")
      param must_== ("100x200")

      "with inline color definition" in {
        val (path :: colorOperation :: colorParam :: borderOperation :: borderParam :: Nil) = border(Geometry(150, 150), red).arguments.toList
        path must endWith("fruehling.jpg")
        colorOperation must_== "-border-color"
        colorParam must_== red.spec
        borderOperation must_== "-border"
        borderParam must_== "150x150"
      }
    }

    "charcoal" in {
      val (path :: operation :: param :: Nil) = charcoal(5).arguments.toList
      path must endWith("fruehling.jpg")
      operation must_== ("-charcoal")
      param must_== ("5")
    }

    "chop" in {
      val (path :: operation :: param :: Nil) = chop(Geometry(100, 10)).arguments.toList
      path must endWith("fruehling.jpg")
      operation must_== ("-chop")
      param must_== ("100x10")
    }

    "colors" in {
      val (path :: operation :: param :: Nil) = colors(16).arguments.toList
      path must endWith("fruehling.jpg")
      operation must_== ("-colors")
      param must_== ("16")
    }

    "colorize" in {
      val (path :: operation :: param :: Nil) = colorize(10).arguments.toList
      path must endWith("fruehling.jpg")
      operation must_== ("-colorize")
      param must_== ("10")

      "with red, green and blue values" in {
        val (path :: operation :: param :: Nil) = colorize(blue = 50).arguments.toList
        path must endWith("fruehling.jpg")
        operation must_== ("-colorize")
        param must_== ("0,0,50")
      }
    }

    "colorspace" in {
      import Colorspace._
      val (path :: operation :: param :: Nil) = colorspace(RGB).arguments.toList
      path must endWith("fruehling.jpg")
      operation must_== ("-colorspace")
      param must_== ("RGB")
    }

    "contrast" in {
      val (path :: operation :: lowerContrast :: Nil) = enhanceContrast.reduceContrast.arguments.toList
      path must endWith("fruehling.jpg")
      operation must_== "-contrast"
      lowerContrast must_== "+contrast"
    }

    "crop" in {
      val (path :: operation :: param :: Nil) = crop(Geometry(100, 10) offset (10, 20)).arguments.toList
      path must endWith("fruehling.jpg")
      operation must_== ("-crop")
      param must_== ("100x10+10+20")
    }
  }
}