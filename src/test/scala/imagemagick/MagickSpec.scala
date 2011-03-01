package org.imagemagick

import org.specs.Specification
import Magick._
import java.io.File


/**
 * User: mathias
 * Date: 01.03.11 20:23
 * Time: 20:23
 */

object MagickSpec extends Specification {

  "magick" should {
    "define implicit int x int to size translation" in {
      val size = 100 x 200
      size.width must_== 100
      size.height must_== 200
    }

    "resolve area definition with implicits" in {
      val area = (100 x 200) + 150 + 110
      area.size.width must_== 100
      area.size.height must_== 200
      area.x must_== 150
      area.y must_== 110
    }

    "basic image attributes definition using case class" in {
      val attrs = ImageAttributes()
      attrs.quality must_== None
      attrs.size must_== None
      val qualAttrs = attrs.copy(quality = Some(5))
      qualAttrs.quality must_== Some(5)
      qualAttrs.size must_== None
    }

    "basic image operation" in {
      val in = new File("src/test/resources/fruehling.jpg")
      val out = new File("target/fruehling.jpg")
      Magick().read(in).thumbnail(200 x 150).write(out)
    }
  }

}
