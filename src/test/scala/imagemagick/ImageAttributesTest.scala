package org.imagemagick

import org.specs.Specification
import org.imagemagick.ImageSourceSpec._
import java.io.File

/**
 * User: mathias
 * Date: 06.03.11 21:21
 * Time: 21:21
 */
class ImageAttributesTest extends Specification {
  "image attributes" should {
    "define image settings" in {

      val file = new File("src/test/resources/fruehling.jpg")
      val source = image(file)

      "adjoin" in {
        val (command :: path :: Nil) = source.adjoin.commands
        command must_== "-adjoin"
      }

      "adjoin disabled" in {
        val (command :: path :: Nil) = source.adjoin(singleFile = false).commands
        command must_== "+adjoin"
      }

      "transform" in {
        val (affine :: matrix :: transform :: path :: Nil) = source.transform(1, 2, 3, 4, 5, 6).commands
        affine must_== "-affine"
        matrix must_== "1.0,2.0,3.0,4.0,5.0,6.0"
        transform must_== "-transform"
      }

      "alpha" in {
        val (command :: param :: path :: Nil) = source.alpha(Alpha.Activate).commands
        command must_== "-alpha"
        param must_== "Activate"
      }

      "antialias" in {
        val (command :: path :: Nil) = source.antialias.commands
        command must_== "-antialias"
      }

      "antialias disabled" in {
        val (command :: path :: Nil) = source.antialias(activate = false).commands
        command must_== "+antialias"
      }

      "authenticate" in {
        val (command :: param :: path :: Nil) = source.authenticate("foo").commands
        command must_== "-authenticate"
        param must_== "foo"
      }

      "background" in {
        val (command :: param :: path :: Nil) = source.background(Color.black).commands
        command must_== "-background"
        param must_== Color.black.spec
      }

      "black point compensation" in {
        val (command :: path :: Nil) = source.blackPointCompensation.commands
        command must_== "-black-point-compensation"
      }

      "black point compensation disabled" in {
        val (command :: path :: Nil) = source.blackPointCompensation(activate = false).commands
        command must_== "+black-point-compensation"
      }

      "blue primary" in {
        val (command :: param :: path :: Nil) = source.bluePrimary(11, 22).commands
        command must_== "-blue-primary"
        param must_== "11,22"
      }

      "border-color" in {
        val (command :: param :: path :: Nil) = source.borderColor(Color.white).commands
        command must_== "-border-color"
        param must_== Color.white.spec
      }

      "caption" in {
        val (command :: param :: path :: Nil) = source.caption("bar").commands
        command must_== "-caption"
        param must_== "bar"
      }

      "channel" in {
        import Channel._
        val (command :: param :: path :: Nil) = source.channel(Red, Blue).commands
        command must_== "-channel"
        param must_== "Red,Blue"
      }

      "comment" in {
        val (command :: param :: path :: Nil) = source.comment("foobar").commands
        command must_== "-comment"
        param must_== "foobar"
      }

      "compress" in {
        import Compression._
        val (command :: param :: path :: Nil) = source.compress(JPEG2000).commands
        command must_== "-compress"
        param must_== "JPEG2000"
      }

      "uncompressed" in {
        import Compression._
        val (command :: path :: Nil) = source.compress(None).commands
        command must_== "+compress"
      }

      "delay" in {
        val delaySource = source delay 10
        val (command :: param :: path :: Nil) = delaySource.commands
        command must_== "-delay"
        param must_== "10"

        "define other settings after delay" in {
          val (delayCommand :: delayParam :: sizeCommand :: sizeParam :: path :: Nil) = delaySource.size(11, 22).commands
          delayCommand must_== "-delay"
          delayParam must_== "10"
          sizeCommand must_== "-size"
          sizeParam must_== "11x22"
        }

        "change only if current value exceeds given delay" in {
          val delaySource = source.delay(10).>
          val (command :: param :: path :: Nil) = delaySource.commands
          command must_== "-delay"
          param must_== "10>"
        }

        "change only if current value less than given delay" in {
          val delaySource = source.delay(10).<
          val (command :: param :: path :: Nil) = delaySource.commands
          command must_== "-delay"
          param must_== "10<"
        }

        "ticks per seconds" in {
          val delayWithTps = source.delay(10, 55)
          val (command :: param :: path :: Nil) = delayWithTps.commands
          command must_== "-delay"
          param must_== "10x55"

          "condition and folowing parameter" in {
            val (delayCommand :: delayParam :: adjoinCommand :: path :: Nil) = delayWithTps.onlyIfLowerThanCurrent.adjoin.commands
            delayCommand must_== "-delay"
            delayParam must_== "10x55>"
            adjoinCommand must_== "-adjoin"
          }
        }
      }

      "density" in {
        "width" in {
          val (command :: param :: path :: Nil) = source.density(100).commands
          command must_== "-density"
          param must_== "100"
        }
        "width and height" in {
          val (command :: param :: path :: Nil) = source.density(100, 200).commands
          command must_== "-density"
          param must_== "100x200"
        }
      }

      "direction" in {

        "rtl" in {
          import Direction._
          val (command :: param :: path :: Nil) = source.direction(rtl).commands
          command must_== "-direction"
          param must_== "right-to-left"
        }

        "ltr" in {
          import Direction._
          val (command :: param :: path :: Nil) = source.direction(ltr).commands
          command must_== "-direction"
          param must_== "left-to-right"
        }
      }

      "dispose" in {
        import Dispose._
        val (command :: param :: path :: Nil) = source.dispose(Background).commands
        command must_== "-dispose"
        param must_== "Background"
      }

      "dither" in {
        import Dither._
        val (command :: param :: path :: Nil) = source.dither(FlyedSteinberg).commands
        command must_== "-dither"
        param must_== "FlyedSteinberg"
      }

      "encoding" in {
        import Encoding._
        val (command :: param :: path :: Nil) = source.encoding(Latin2).commands
        command must_== "-encoding"
        param must_== "Latin 2"
      }

      "endian" in {
        import Endian._
        val (command :: param :: path :: Nil) = source.endian(MSB).commands
        command must_== "-endian"
        param must_== "MSB"
      }

      "extract" in {
        val (command :: param :: path :: Nil) = source.extract(Geometry(1000).area).commands
        command must_== "-extract"
        param must_== "1000@"
      }

      "family" in {
        val (command :: param :: path :: Nil) = source.family("Arial").commands
        command must_== "-family"
        param must_== "Arial"
      }

      "fill" in {
        val (command :: param :: path :: Nil) = source.fill(Color.white).commands
        command must_== "-fill"
        param must_== Color.white.spec
      }

      "filter" in {
        import Filter._
        val filter = source.filter(Mitchell)
        val (command :: param :: path :: Nil) = filter.commands
        command must_== "-filter"
        param must_== "Mitchell"

        "blur" in {
          val (command :: param :: define :: defineParam :: path :: Nil) = filter.blur(.5).commands
          command must_== "-filter"
          param must_== "Mitchell"
          define must_== "-define"
          defineParam must_== "filter:blur=0.5"

          "continue defining other settings" in {
            val settings = filter.blur(.5).background(Color.black)
            val (command :: param :: define :: defineParam :: background :: color :: path :: Nil) = settings.commands
            command must_== "-filter"
            param must_== "Mitchell"
            define must_== "-define"
            defineParam must_== "filter:blur=0.5"
            background must_== "-background"
            color must_== Color.black.spec
          }
        }
      }

      "font" in {
        val (command :: param :: path :: Nil) = source.font("Helvetica").commands
        command must_== "-font"
        param must_== "Helvetica"
      }

      "format" in {
        val (command :: param :: path :: Nil) = source.format("gif").commands
        command must_== "-format"
        param must_== "gif"
      }

      "fuzz" in {
        val (command :: param :: path :: Nil) = source.fuzz(10).commands
        command must_== "-fuzz"
        param must_== "10"

        "percent" in {
          val (command :: param :: path :: Nil) = source.fuzz(Percent(2)).commands
          command must_== "-fuzz"
          param must_== "2.0%"
        }
      }

      "geometry" in {
        val (command :: param :: path :: Nil) = source.geometry(Geometry(123, 456).offset(7, 8)).commands
        command must_== "-geometry"
        param must_== "123x456+7+8"
      }

      "gravity" in {
        val (command :: param :: path :: Nil) = source.gravity(Gravity.Center).commands
        command must_== "-gravity"
        param must_== "Center"
      }

      "green-primary" in {
        val (command :: param :: path :: Nil) = source.greenPrimary(3, 4).commands
        command must_== "-green-primary"
        param must_== "3,4"
      }

      "interlace" in {
        val (command :: param :: path :: Nil) = source.interlace(Interlace.Partition).commands
        command must_== "-interlace"
        param must_== "Partition"
      }

      "intent" in {
        val (command :: param :: path :: Nil) = source.intent(Intent.Relative).commands
        command must_== "-intent"
        param must_== "Relative"
      }

      "interpolate" in {
        val (command :: param :: path :: Nil) = source.interpolate(Interpolation.Bilinear).commands
        command must_== "-interpolate"
        param must_== "Bilinear"
      }

      "label" in {
        val (command :: param :: path :: Nil) = source.label("%m:%f %wx%h").commands
        command must_== "-label"
        param must_== "%m:%f %wx%h"

        "reset" in {
          val (command :: path :: Nil) = source.label.reset.commands
          command must_== "+label"
        }
      }

      "limit" in {
        import CacheResource._
        import SizeConversions._
        val (command :: param :: amount :: path :: Nil) = source.limit(Memory, 2.12 KB).commands
        command must_== "-limit"
        param must_== "Memory"
        amount must_== (2.12 * 1024).toInt.toString
      }

      "loop" in {
        val (command :: param :: path :: Nil) = source.loop(10).commands
        command must_== "-loop"
        param must_== "10"
      }

      "mask" in {
        val maskFile = new File("foo")
        val (command :: param :: path :: Nil) = source.mask(maskFile).commands
        command must_== "-mask"
        param must_== maskFile.getAbsolutePath

        "reset" in {
          val (command :: path :: Nil) = source.mask.reset.commands
          command must_== "+mask"
        }
      }

      "mattcolor" in {
        val (command :: param :: path :: Nil) = source.mattcolor(Color.green).commands
        command must_== "-mattcolor"
        param must_== Color.green.spec
      }

      "orient" in {
        import ImageOrientation._
        val (command :: param :: path :: Nil) = source.orient(BottomRight).commands
        command must_== "-orient"
        param must_== "BottomRight"
      }

      "page" in {
        "geometry" in {
          val (command :: param :: path :: Nil) = source.page(Geometry(100, 200)).commands
          command must_== "-page"
          param must_== "100x200"
        }

        "media" in {
          import Media._
          val (command :: param :: path :: Nil) = source.page(Letter(-5, -4).<).commands
          command must_== "-page"
          param must_== "Letter-5-4<"
        }

        "deactivate" in {
          val (command :: path :: Nil) = source.page.commands
          command must_== "+page"
        }
      }

      "pointsize" in {
        val (command :: param :: path :: Nil) = source.pointsize(10).commands
        command must_== "-pointsize"
        param must_== "10"
      }

      "preview" in {
        import Preview._
        val (command :: param :: path :: Nil) = source.preview(Wave).commands
        command must_== "-preview"
        param must_== "Wave"
      }

      "quality" in {
        val (command :: param :: path :: Nil) = source.quality(75).commands
        command must_== "-quality"
        param must_== "75"
      }

      "red primary" in {
        val (command :: param :: path :: Nil) = source.redPrimary(7, 5).commands
        command must_== "-red-primary"
        param must_== "7,5"
      }

      "region" in {
        val (command :: param :: path :: Nil) = source.region(Geometry(11, 22)).commands
        command must_== "-region"
        param must_== "11x22"
      }

      "render" in {
        val (command :: path :: Nil) = source.render.commands
        command must_== "-render"

        "deactivate" in {
          val (command :: path :: Nil) = source.render(false).commands
          command must_== "+render"
        }
      }

      "repage" in {
        val (command :: param :: path :: Nil) = source.repage(Geometry(33, 44)).commands
        command must_== "-repage"
        param must_== "33x44"

        "reset" in {
          val (command :: path :: Nil) = source.repage.reset.commands
          command must_== "+repage"
        }
      }

      "scene" in {
        val (command :: param :: path :: Nil) = source.scene(3).commands
        command must_== "-scene"
        param must_== "3"
      }

      "seed" in {
        val (command :: path :: Nil) = source.seed.commands
        command must_== "-seed"
      }

      "strech" in {
        import FontStretch._
        val (command :: param :: path :: Nil) = source.strech(UltraExpanded).commands
        command must_== "-strech"
        param must_== "UltraExpanded"
      }

      "stroke" in {
        val (command :: param :: path :: Nil) = source.stroke(Color.black).commands
        command must_== "-stroke"
        param must_== Color.black.spec
      }

      "strokewidth" in {
        val (command :: param :: path :: Nil) = source.strokewidth(10).commands
        command must_== "-strokewidth"
        param must_== "10"
      }

      "style" in {
        import FontStyle._
        val (command :: param :: path :: Nil) = source.style(Oblique).commands
        command must_== "-style"
        param must_== "Oblique"
      }

      "texture" in {
        val file = new File("blub")
        val (command :: param :: path :: Nil) = source.texture(file).commands
        command must_== "-texture"
        param must_== file.getAbsolutePath
      }

      "transparent color" in {
        val (command :: param :: path :: Nil) = source.transparentColor(Color.red).commands
        command must_== "-transparent-color"
        param must_== Color.red.spec
      }

      "treedepth" in {
        import FontStyle._
        val (command :: param :: path :: Nil) = source.treedepth(1).commands
        command must_== "-treedepth"
        param must_== "1"
      }

      "type" in {
        import ImageType._
        val (command :: param :: path :: Nil) = source.imagetype(Grayscale).commands
        command must_== "-type"
        param must_== "Grayscale"
      }

      "undercolor" in {
        val (command :: param :: path :: Nil) = source.undercolor(Color.blue).commands
        command must_== "-undercolor"
        param must_== Color.blue.spec
      }

      "units" in {
        import ImageResolutionUnit._
        val (command :: param :: path :: Nil) = source.units(PixelsPerCentimeter).commands
        command must_== "-units"
        param must_== "PixelsPerCentimeter"
      }

      "virtual pixel" in {
        import VirtualPixelMethod._
        val (command :: param :: path :: Nil) = source.virtualPixel(VerticalTile).commands
        command must_== "-virtual-pixel"
        param must_== "VerticalTile"
      }

      "weight" in {
        import FontWeight._
        val (command :: param :: path :: Nil) = source.weight(Bolder).commands
        command must_== "-weight"
        param must_== "Bolder"
      }

    }
  }
}