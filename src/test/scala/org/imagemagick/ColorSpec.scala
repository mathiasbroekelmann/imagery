package org.imagemagick

import org.specs.Specification
import Color._

/**
 * User: mathias
 * Date: 02.03.11 22:12
 * Time: 22:12
 */

class ColorSpec extends Specification {

  "Color" should {
    "define named colors" in {
      Color("black").spec must_== "black"
    }

    "define rgb colors using hex" in {
      Color("#ff0000").spec must_== "#ff0000"
    }

    "define rgb colors" in {
      Color(0, 127, 255).spec must_== "rgb(0,127,255)"
    }

    "define rgb colors with 0 to 255 ranges" in {
      Color(-100, 12312313, 0).spec must_== "rgb(0,255,0)"
    }


    "define rgb colors with " in {
      Color(0, 127, 255).spec must_== "rgb(0,127,255)"
    }
  }
}