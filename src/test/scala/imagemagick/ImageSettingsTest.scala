package org.imagemagick

import org.specs.Specification
import java.io.File

/**
 * User: mathias
 * Date: 06.03.11 21:21
 * Time: 21:21
 */
class ImageSettingsTest extends Specification {
  "image attributes" should {
    "define image settings" in {

      import Convert._

      val file = new File("src/test/resources/fruehling.jpg")
      val source = image(file)

      "adjoin" in {
        val (command :: path :: Nil) = adjoin.apply(file).arguments
        command must_== "-adjoin"
      }

      "adjoin disabled" in {
        val (command :: path :: Nil) = adjoin(singleFile = false).apply(file).arguments
        command must_== "+adjoin"
      }

      "transform" in {
        val args: Iterable[String] = transform(1, 2, 3, 4, 5, 6).apply(file).arguments
        val (affine :: matrix :: trans :: path :: Nil) = args
        affine must_== "-affine"
        matrix must_== "1.0,2.0,3.0,4.0,5.0,6.0"
        trans must_== "-transform"
      }

      "alpha" in {
        val (command :: param :: path :: Nil) = alpha(Alpha.Activate).apply(file).arguments
        command must_== "-alpha"
        param must_== "Activate"
      }

      "antialias" in {
        val (command :: path :: Nil) = antialias.apply(file).arguments
        command must_== "-antialias"
      }

      "antialias disabled" in {
        val (command :: path :: Nil) = antialias(activate = false).apply(file).arguments
        command must_== "+antialias"
      }

      "authenticate" in {
        val (command :: param :: path :: Nil) = authenticate("foo").apply(file).arguments
        command must_== "-authenticate"
        param must_== "foo"
      }

      "background" in {
        val (command :: param :: path :: Nil) = background(Color.black).apply(file).arguments
        command must_== "-background"
        param must_== Color.black.spec
      }

      "black point compensation" in {
        val (command :: path :: Nil) = blackPointCompensation.apply(file).arguments
        command must_== "-black-point-compensation"
      }

      "black point compensation disabled" in {
        val (command :: path :: Nil) = blackPointCompensation(activate = false).apply(file).arguments
        command must_== "+black-point-compensation"
      }

      "blue primary" in {
        val (command :: param :: path :: Nil) = bluePrimary(11, 22).apply(file).arguments
        command must_== "-blue-primary"
        param must_== "11,22"
      }

      "border-color" in {
        val (command :: param :: path :: Nil) = borderColor(Color.white).apply(file).arguments
        command must_== "-border-color"
        param must_== Color.white.spec
      }

      "caption" in {
        val (command :: param :: path :: Nil) = caption("bar").apply(file).arguments
        command must_== "-caption"
        param must_== "bar"
      }

      "channel" in {
        import Channel._
        val (command :: param :: path :: Nil) = channel(Red, Blue).apply(file).arguments
        command must_== "-channel"
        param must_== "Red,Blue"
      }

      "comment" in {
        val (command :: param :: path :: Nil) = comment("foobar").apply(file).arguments
        command must_== "-comment"
        param must_== "foobar"
      }

      "compress" in {
        import Compression._
        val (command :: param :: path :: Nil) = compress(JPEG2000).apply(file).arguments
        command must_== "-compress"
        param must_== "JPEG2000"
      }

      "uncompressed" in {
        import Compression._
        val (command :: path :: Nil) = compress(None).apply(file).arguments
        command must_== "+compress"
      }

      "delay" in {
        val delaySource = delay(10).apply(file)
        val (command :: param :: path :: Nil) = delaySource.arguments
        command must_== "-delay"
        param must_== "10"

        "define other settings after delay" in {
          val sized = delay(10).size(11, 22).apply(file)
          val (delayCommand :: delayParam :: sizeCommand :: sizeParam :: path :: Nil) = sized.arguments
          delayCommand must_== "-delay"
          delayParam must_== "10"
          sizeCommand must_== "-size"
          sizeParam must_== "11x22"
        }

        "change only if current value exceeds given delay" in {
          val delaySource = delay(10, delay = Some(Delay.>)).apply(file)
          val (command :: param :: path :: Nil) = delaySource.arguments
          command must_== "-delay"
          param must_== "10>"
        }

        "change only if current value less than given delay" in {
          val delaySource = delay(10, delay = Some(Delay.<)).apply(file)
          val (command :: param :: path :: Nil) = delaySource.arguments
          command must_== "-delay"
          param must_== "10<"
        }

        "ticks per seconds" in {
          val delayWithTps = delay(10, 55).apply(file)
          val (command :: param :: path :: Nil) = delayWithTps.arguments
          command must_== "-delay"
          param must_== "10x55"

          "condition and folowing parameter" in {
            val (delayCommand :: delayParam :: adjoinCommand :: path :: Nil) = delay(10, 55).adjoin.apply(file).arguments
            delayCommand must_== "-delay"
            delayParam must_== "10x55"
            adjoinCommand must_== "-adjoin"
          }
        }
      }

      "density" in {
        "width" in {
          val (command :: param :: path :: Nil) = density(100).apply(file).arguments
          command must_== "-density"
          param must_== "100"
        }
        "width and height" in {
          val (command :: param :: path :: Nil) = density(100, 200).apply(file).arguments
          command must_== "-density"
          param must_== "100x200"
        }
      }

      "direction" in {

        "rtl" in {
          import Direction._
          val (command :: param :: path :: Nil) = direction(rtl).apply(file).arguments
          command must_== "-direction"
          param must_== "right-to-left"
        }

        "ltr" in {
          import Direction._
          val (command :: param :: path :: Nil) = direction(ltr).apply(file).arguments
          command must_== "-direction"
          param must_== "left-to-right"
        }
      }

      "dispose" in {
        import Dispose._
        val (command :: param :: path :: Nil) = dispose(Background).apply(file).arguments
        command must_== "-dispose"
        param must_== "Background"
      }

      "dither" in {
        import Dither._
        val (command :: param :: path :: Nil) = dither(FlyedSteinberg).apply(file).arguments
        command must_== "-dither"
        param must_== "FlyedSteinberg"
      }

      "encoding" in {
        import Encoding._
        val (command :: param :: path :: Nil) = encoding(Latin2).apply(file).arguments
        command must_== "-encoding"
        param must_== "Latin 2"
      }

      "endian" in {
        import Endian._
        val (command :: param :: path :: Nil) = endian(MSB).apply(file).arguments
        command must_== "-endian"
        param must_== "MSB"
      }

      "extract" in {
        val (command :: param :: path :: Nil) = extract(Geometry(1000).area).apply(file).arguments
        command must_== "-extract"
        param must_== "1000@"
      }

      "family" in {
        val (command :: param :: path :: Nil) = family("Arial").apply(file).arguments
        command must_== "-family"
        param must_== "Arial"
      }

      "fill" in {
        val (command :: param :: path :: Nil) = fill(Color.white).apply(file).arguments
        command must_== "-fill"
        param must_== Color.white.spec
      }

      "filter" in {
        import FilterType._
        val filter = Convert.filter(Mitchell).apply(file)
        val (command :: param :: path :: Nil) = filter.arguments
        command must_== "-filter"
        param must_== "Mitchell"

        "blur" in {
          val (command :: param :: define :: defineParam :: path :: Nil) = Convert.filter(Filter(Mitchell).blur(.5)).apply(file).arguments
          command must_== "-filter"
          param must_== "Mitchell"
          define must_== "-define"
          defineParam must_== "filter:blur=0.5"

          "continue defining other settings" in {
            val settings = Convert.filter(Filter(Mitchell).blur(.5)).background(Color.black).apply(file)
            val (command :: param :: define :: defineParam :: background :: color :: path :: Nil) = settings.arguments
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
        val (command :: param :: path :: Nil) = font("Helvetica").apply(file).arguments
        command must_== "-font"
        param must_== "Helvetica"
      }

      "format" in {
        val (command :: param :: path :: Nil) = format("gif").apply(file).arguments
        command must_== "-format"
        param must_== "gif"
      }

      "fuzz" in {
        val (command :: param :: path :: Nil) = fuzz(10).apply(file).arguments
        command must_== "-fuzz"
        param must_== "10"

        "percent" in {
          val (command :: param :: path :: Nil) = fuzz(Percent(2)).apply(file).arguments
          command must_== "-fuzz"
          param must_== "2.0%"
        }
      }

      "geometry" in {
        val (command :: param :: path :: Nil) = geometry(Geometry(123, 456).offset(7, 8)).apply(file).arguments
        command must_== "-geometry"
        param must_== "123x456+7+8"
      }

      "gravity" in {
        val (command :: param :: path :: Nil) = gravity(Gravity.Center).apply(file).arguments
        command must_== "-gravity"
        param must_== "Center"
      }

      "green-primary" in {
        val (command :: param :: path :: Nil) = greenPrimary(3, 4).apply(file).arguments
        command must_== "-green-primary"
        param must_== "3,4"
      }

      "interlace" in {
        val (command :: param :: path :: Nil) = interlace(Interlace.Partition).apply(file).arguments
        command must_== "-interlace"
        param must_== "Partition"
      }

      "intent" in {
        val (command :: param :: path :: Nil) = intent(Intent.Relative).apply(file).arguments
        command must_== "-intent"
        param must_== "Relative"
      }

      "interpolate" in {
        val (command :: param :: path :: Nil) = interpolate(Interpolation.Bilinear).apply(file).arguments
        command must_== "-interpolate"
        param must_== "Bilinear"
      }

      "label" in {
        val (command :: param :: path :: Nil) = label("%m:%f %wx%h").apply(file).arguments
        command must_== "-label"
        param must_== "%m:%f %wx%h"

        "reset" in {
          val (command :: path :: Nil) = label.reset.apply(file).arguments
          command must_== "+label"
        }
      }

      "limit" in {
        import CacheResource._
        import SizeConversions._
        val (command :: param :: amount :: path :: Nil) = limit(Memory, 2.12 KB).apply(file).arguments
        command must_== "-limit"
        param must_== "Memory"
        amount must_== (2.12 * 1024).toInt.toString
      }

      "loop" in {
        val (command :: param :: path :: Nil) = loop(10).apply(file).arguments
        command must_== "-loop"
        param must_== "10"
      }

      "mask" in {
        val maskFile = new File("foo")
        val (command :: param :: path :: Nil) = mask(maskFile).apply(file).arguments
        command must_== "-mask"
        param must_== maskFile.getAbsolutePath

        "reset" in {
          val (command :: path :: Nil) = mask.reset.apply(file).arguments
          command must_== "+mask"
        }
      }

      "mattcolor" in {
        val (command :: param :: path :: Nil) = mattcolor(Color.green).apply(file).arguments
        command must_== "-mattcolor"
        param must_== Color.green.spec
      }

      "orient" in {
        import ImageOrientation._
        val (command :: param :: path :: Nil) = orient(BottomRight).apply(file).arguments
        command must_== "-orient"
        param must_== "BottomRight"
      }

      "page" in {
        "geometry" in {
          val (command :: param :: path :: Nil) = page(Geometry(100, 200)).apply(file).arguments
          command must_== "-page"
          param must_== "100x200"
        }

        "media" in {
          import Media._
          val (command :: param :: path :: Nil) = page(Letter(-5, -4).<).apply(file).arguments
          command must_== "-page"
          param must_== "Letter-5-4<"
        }

        "deactivate" in {
          val (command :: path :: Nil) = page.apply(file).arguments
          command must_== "+page"
        }
      }

      "pointsize" in {
        val (command :: param :: path :: Nil) = pointsize(10).apply(file).arguments
        command must_== "-pointsize"
        param must_== "10"
      }

      "preview" in {
        import Preview._
        val (command :: param :: path :: Nil) = preview(Wave).apply(file).arguments
        command must_== "-preview"
        param must_== "Wave"
      }

      "quality" in {
        val (command :: param :: path :: Nil) = quality(75).apply(file).arguments
        command must_== "-quality"
        param must_== "75"
      }

      "red primary" in {
        val (command :: param :: path :: Nil) = redPrimary(7, 5).apply(file).arguments
        command must_== "-red-primary"
        param must_== "7,5"
      }

      "region" in {
        val (command :: param :: path :: Nil) = region(Geometry(11, 22)).apply(file).arguments
        command must_== "-region"
        param must_== "11x22"
      }

      "render" in {
        val (command :: path :: Nil) = render.apply(file).arguments
        command must_== "-render"

        "deactivate" in {
          val (command :: path :: Nil) = render(false).apply(file).arguments
          command must_== "+render"
        }
      }

      "repage" in {
        val (command :: param :: path :: Nil) = repage(Geometry(33, 44)).apply(file).arguments
        command must_== "-repage"
        param must_== "33x44"

        "reset" in {
          val (command :: path :: Nil) = repage.reset.apply(file).arguments
          command must_== "+repage"
        }
      }

      "scene" in {
        val (command :: param :: path :: Nil) = scene(3).apply(file).arguments
        command must_== "-scene"
        param must_== "3"
      }

      "seed" in {
        val (command :: path :: Nil) = seed.apply(file).arguments
        command must_== "-seed"
      }

      "strech" in {
        import FontStretch._
        val (command :: param :: path :: Nil) = strech(UltraExpanded).apply(file).arguments
        command must_== "-strech"
        param must_== "UltraExpanded"
      }

      "stroke" in {
        val (command :: param :: path :: Nil) = stroke(Color.black).apply(file).arguments
        command must_== "-stroke"
        param must_== Color.black.spec
      }

      "strokewidth" in {
        val (command :: param :: path :: Nil) = strokewidth(10).apply(file).arguments
        command must_== "-strokewidth"
        param must_== "10"
      }

      "style" in {
        import FontStyle._
        val (command :: param :: path :: Nil) = style(Oblique).apply(file).arguments
        command must_== "-style"
        param must_== "Oblique"
      }

      "texture" in {
        val file = new File("blub")
        val (command :: param :: path :: Nil) = texture(file).apply(file).arguments
        command must_== "-texture"
        param must_== file.getAbsolutePath
      }

      "transparent color" in {
        val (command :: param :: path :: Nil) = transparentColor(Color.red).apply(file).arguments
        command must_== "-transparent-color"
        param must_== Color.red.spec
      }

      "treedepth" in {
        import FontStyle._
        val (command :: param :: path :: Nil) = treedepth(1).apply(file).arguments
        command must_== "-treedepth"
        param must_== "1"
      }

      "type" in {
        import ImageType._
        val (command :: param :: path :: Nil) = imagetype(Grayscale).apply(file).arguments
        command must_== "-type"
        param must_== "Grayscale"
      }

      "undercolor" in {
        val (command :: param :: path :: Nil) = undercolor(Color.blue).apply(file).arguments
        command must_== "-undercolor"
        param must_== Color.blue.spec
      }

      "units" in {
        import ImageResolutionUnit._
        val (command :: param :: path :: Nil) = units(PixelsPerCentimeter).apply(file).arguments
        command must_== "-units"
        param must_== "PixelsPerCentimeter"
      }

      "virtual pixel" in {
        import VirtualPixelMethod._
        val (command :: param :: path :: Nil) = virtualPixel(VerticalTile).apply(file).arguments
        command must_== "-virtual-pixel"
        param must_== "VerticalTile"
      }

      "weight" in {
        import FontWeight._
        val (command :: param :: path :: Nil) = weight(Bolder).apply(file).arguments
        command must_== "-weight"
        param must_== "Bolder"
      }

    }
  }
}