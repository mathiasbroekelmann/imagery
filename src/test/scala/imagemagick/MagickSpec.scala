package org.imagemagick

import org.specs.Specification
import java.io.File
import org.imagemagick.Magick._

/**
 * User: mathias
 * Date: 01.03.11 20:23
 * Time: 20:23
 */
object MagickSpec extends Specification {

  "magick" should {
    "define implicit int x int to size translation" in {
      val size = width(100).height(200)
      size.width must_== 100
      size.height must_== 200
    }

    "resolve area definition with positive offsets" in {
      val area = width(100) height(200) x(150) y(110)
      area.width must_== 100
      area.height must_== 200
      area.x must_== 150
      area.y must_== 110
    }

    "resolve area definition with mixed positive/negative offsets" in {
      val size = width(100) height(200)
      size.x(150).y(110).x must_== 150
      size.x(-150).y(110).x must_== -150
    }

    "basic image attributes definition using case class" in {
      val attrs = ImageAttributes()
      attrs.quality must_== None
      attrs.size must_== None
      val qualAttrs = attrs.copy(quality = Some(5))
      qualAttrs.quality must_== Some(5)
      qualAttrs.size must_== None
    }

    val in = new File("src/test/resources/fruehling.jpg")
    val out = new File("target/fruehling.jpg")

    "thumbnail image operation" in {
      read(in).thumbnail(width(100) height(200)).write(out)
      out mustVerify(_.exists)
    }

    "size attribute definition" in {
      val s = Magick.size -> (width(100) height(200))
      val (param :: arg :: Nil) = s.commands.toList
      param must_== "-size"
      arg must_== "100x200"
    }

    "size with thumbnail operation" in {
      convert(Magick.size -> (width(500) height(500))).read(in).thumbnail(width(50).height(50)).write(out)
    }

    "unsharp operation" in {
      val v = radius(0).x(.5) +(1) +(.05)
      v.commands.head must_== "0x0.5+1.0+0.05"
    }
  }
}
