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
      read(in).thumbnail(Geometry(100, 200)).write(out)
      out mustVerify (_.exists)
    }

    "size with thumbnail operation" in {
      convert(
        Magick.size(width = 500, height = 500),
        quality(95)
      ).read(in).thumbnail(Geometry(width = 50, height = 50))
    }

    "background operation" in {
      val (file :: command :: param :: Nil) = read(in).background(transparent).commands.toList
      command must_== "-background"
      param must_== "#00000000"
    }

    "gravity" in {
      val (file :: command :: param :: Nil) = read(in).gravity(Gravity.North).commands.toList
      command must_== "-gravity"
      param must_== "North"
    }

    "thumbnail creation by using pad out the image" in {
      read(in).thumbnail(Geometry(100, 100).>)
        .background(white)
        .gravity(Gravity.Center)
        .extent(Geometry(150, 100))
        .write(new File("target/fruehling_pad_out_tn.jpg"))
      out mustVerify (_.exists)
    }

    "image geometry specification" in {

      import Geometry._

      "width and height" in {
        val heightOnly = Geometry(height = Some(100))
        val widthOnly = Geometry(width = Some(10))
        val widthHeight = Geometry(width = Some(10), height = Some(20))

        heightOnly.spec must_== "x100"
        widthOnly.spec must_== "10"
        widthHeight.spec must_== "10x20"

        "with offset" in {
          heightOnly(100, 200).spec must_== "x100+100+200"
          heightOnly(100, 200).escape must_== false
          heightOnly(-100, -200).spec must_== "x100-100-200"
          heightOnly(x = 100, y = 200).spec must_== "x100+100+200"
          heightOnly.offset(100, 200).spec must_== "x100+100+200"

          widthOnly(10, 22).spec must_== "10+10+22"
          widthOnly.offset(10, -22).spec must_== "10+10-22"

          widthHeight(10, 22).spec must_== "10x20+10+22"
          widthHeight.offset(10, 22).spec must_== "10x20+10+22"
        }
      }

      "scale" in {
        val scale = Geometry(scale = 50)
        val scaleXY = Geometry(scalex = 50, scaley = 70)

        scale.spec must_== "50.0%"
        scale.escape must_== true
        scaleXY.spec must_== "50.0x70.0%"
        scaleXY.escape must_== true

        "with offset" in {
          scale(11, 22).spec must_== "50.0%+11+22"
          scaleXY(21, 32).spec must_== "50.0x70.0%+21+32"

        }
      }

      "conditional width and height" in {
        val wh = Geometry(width = 111, height = 222)

        wh.spec must_== "111x222"
        wh.escape must_== false
        wh.min.spec must_== "111x222^"
        wh.min.escape must_== true
        wh.^.spec must_== "111x222^"

        wh.force.spec must_== "111x222!"
        wh.force.escape must_== true
        wh.!.spec must_== "111x222!"

        wh.ifGreater.spec must_== "111x222>"
        wh.ifGreater.escape must_== true
        wh.>.spec must_== "111x222>"

        wh.ifBothGreater.spec must_== "111x222<"
        wh.ifBothGreater.escape must_== true
        wh.<.spec must_== "111x222<"

        "with offset" in {
          wh(11, 22).spec must_== "111x222+11+22"
          wh.^(21, 32).spec must_== "111x222^+21+32"
          wh.!(21, 32).spec must_== "111x222!+21+32"
          wh.>(21, -32).spec must_== "111x222>+21-32"
          wh.<(-21, 32).spec must_== "111x222<-21+32"
        }
      }

      "area" in {
        val area = Geometry(10000).area

        area.spec must_== "10000@"
        Geometry(100).\@.spec must_== "100@"

        "with offset" in {
          area(11, 22).spec must_== "10000@+11+22"
        }
      }
    }
  }
}
