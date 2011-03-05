package org.imagemagick

import java.io.OutputStream
import org.apache.commons.lang.builder.HashCodeBuilder

/**
 * User: mathias
 * Date: 05.03.11 18:34
 * Time: 18:34
 */

trait ImageAttributes {

  def imageSource: ImageSource

  def attributes: Iterable[ImageAttibute] = Nil

  def apply(attribute: ImageAttibute): ImageSource = ImageSourceWithAttributes(imageSource, attributes ++ List(attribute))

  def depth(value: Int) = {
    assert(value > 0, "Color depth must be greater than 0, but was " + value)
    apply(new ImageAttibute("depth") {
      override def commands = super.commands ::: value.toString :: Nil
    })
  }

  def size(width: Int, height: Int, offset: Option[Int] = None) =
    apply(new ImageAttibute("size") {
      override def commands = super.commands ::: width + "x" + height + offset.map("+" + _.toString).getOrElse("") :: Nil
    })

  /**
   * Join images into a single multi-image file.
   */
  def adjoin: ImageSource = adjoin()

  /**
   * Join images into a single multi-image file.
   * Use #singleFile = false to force each image to be written to separate files
   */
  def adjoin(singleFile: Boolean = true) = apply(new ImageAttibute("adjoin", singleFile))

  /**
   * Applies the given transformation matrix.
   */
  def transform(sx: Double = 0, rx: Double = 0, ry: Double = 0, sy: Double = 0, tx: Double = 0, ty: Double = 0) =
    apply(new ImageAttibute("affine") {
      override def commands = super.commands ::: sx + "," + rx + "," + ry + "," + sy + "," + tx + "," + ty :: "-transform" :: Nil
    })

  /**
   * Gives control of the alpha/matte channel of an image.
   */
  def alpha(alphaType: Alpha.Alpha) = apply(new ImageAttibute("alpha") {
    override def commands = super.commands ::: alphaType.toString :: Nil
  })

  /**
   * Enable of the rendering of anti-aliasing pixels when drawing fonts and lines.
   */
  def antialias: ImageSource = antialias()

  /**
   * Enable/Disable of the rendering of anti-aliasing pixels when drawing fonts and lines.
   */
  def antialias(activate: Boolean = true) = apply(new ImageAttibute("antialias", activate))

  /**
   * Decrypt a PDF with a password.
   * Use this option to supply a password for decrypting a PDF that has been encrypted using Microsoft Crypto API (MSC API).
   *
   * For a different encryption method, see encipher and decipher.
   */
  def authenticate(password: String) = apply(new ImageAttibute("authenticate") {
    override def commands = super.commands ::: password :: Nil
  })

  /**
   * Set the background color.
   *
   * The color is specified using the format described under the fill option.
   * The default background color (if none is specified or found in the image) is white.
   */
  def background(color: Color) = apply(new ImageAttibute("background") {
    override def commands = super.commands ::: color.spec :: Nil
  })

  // def bias

  /**
   * Use black point compensation.
   */
  def blackPointCompensation: ImageSource = blackPointCompensation()

  /**
   * Enable/Disable black point compensation.
   */
  def blackPointCompensation(activate: Boolean = true) = apply(new ImageAttibute("black-point-compensation", activate))

  /**
   * Set the blue chromaticity primary point.
   */
  def bluePrimary(x: Int, y: Int) = apply(new ImageAttibute("blue-primary") {
    override def commands = super.commands ::: x + "," + y :: Nil
  })

  /**
   * Set the border color.
   *
   * The default border color is #DFDFDF, this shade of gray.
   */
  def borderColor(color: Color) = apply(new ImageAttibute("border-color") {
    override def commands = super.commands ::: color.spec :: Nil
  })

  /**
   * The caption can contain special format characters listed in the Format and Print Image Properties.
   * These attributes are expanded when the caption is finally assigned to the individual images.
   *
   * If the first character of string is @, the image caption is read from a file titled by the remaining
   * characters in the string. Comments read in from a file are literal; no embedded formatting characters are recognized.
   */
  def caption(caption: String) = apply(new ImageAttibute("caption") {
    override def commands = super.commands ::: caption :: Nil
  })

  /**
   * Specify those image color channels to which subsequent operators are limited.
   */
  def channel(channel: Channel.Channel, moreChannels: Channel.Channel*) = apply(new ImageAttibute("channel") {

    lazy val spec = channel :: Option(moreChannels).map(_.toList).getOrElse(Nil)

    override def commands = super.commands ::: spec.mkString(",") :: Nil
  })

  /**
   * Embed a comment in an image.
   */
  def comment(comment: String) = apply(new ImageAttibute("comment") {
    override def commands = super.commands ::: comment :: Nil
  })

  /**
   * Use pixel compression specified by type when writing the image.
   */
  def compress(compressType: Compression.Value): ImageSource = compress(Option(compressType))

  /**
   * Use pixel compression specified by type when writing the image.
   * Use None to store the binary image in an uncompressed format.
   */
  def compress(compressType: Option[Compression.Value] = None) = apply(new ImageAttibute("compress", compressType.isDefined) {
    override def commands = compressType match {
      case Some(c) => super.commands ::: c.toString :: Nil
      case _ => super.commands
    }
  })

  def define(settings: Definitions) = {}
}

trait Definitions {
  /**
   * Set the display range to the minimum and maximum pixel values for the DCM image format.
   */
  def dcmDisplayRangeReset: Definitions

  /**
   * Set the specify the layout engine for the DOT image format (e.g. neato).
   */
  def dotLayoutEngine(value: String): Definitions

  /**
   * Restrict the maximum JPEG file size, for example 400000 bytes.
   */
  def jpegExtend(bytes: Int): Definitions

  /**
   * Set the size hint of a JPEG image, for example, 128x128.
   * It is most useful for increasing performance and reducing the memory requirements when reducing the size of a
   * large JPEG image.
   */
  def jpegSize(geometry: ImageGeometry): Definitions

  /**
   * Specify the compression factor to use while writing JPEG-2000 files.
   * The compression factor is the reciprocal of the compression ratio.
   * The valid range is 0.0 to 1.0, with 1.0 indicating lossless compression.
   * If defined, this value overrides the quality setting.
   * A quality setting of 75 results in a rate value of 0.06641.
   */
  def jp2Rate(value: Double): Definitions

  /**
   * turn playback caching off for streaming MNG.
   */
  def mngNeedCacheoff: Definitions

  /**
   * desired bit-depth and color-type for PNG output.
   * You can force the PNG encoder to use a different bit-depth and color-type than it would have normally selected,
   * but only if this does not cause any loss of image quality.
   *
   * Any attempt to reduce image quality is treated as an error and no PNG file is written.
   * E.g., if you have a 1-bit black-and-white image, you can use these "defines" to cause it to be written
   * as an 8-bit grayscale, indexed, or even a 64-bit RGBA. But if you have a 16-million color image,
   * you cannot force it to be written as a grayscale or indexed PNG.
   *
   * If you wish to do this, you must use the appropriate -depth, -colors, or -type directives to
   * reduce the image quality prior to using the PNG encoder. Note that in indexed PNG files, "bit-depth" refers to
   * the number of bits per index, which can be 1, 2, 4, or 8. In such files, the color samples always have 8-bit depth.
   */
  def pngBitDepth(depth: Int): Definitions

  
}

/*



  def define

  def delay

  def density

  def depth

  def direction

  def display

  def dispose

  def dither

  def encoding

  def endian

  def extract

  def family

  def fill

  def filter

  def font

  def format

  def fuzz

  def geometry

  def gravity

  def greenPrimary

  def interlace

  def intent

  def interpolate

  def label

  def limit

  def linewidth

  //def log

  def loop

  def mask

  def mattcolor

  def monitor

  def orient

  def page

  def pointsize

  def preview

  def quality

  //def quiet

  def redPrimary

  def region

  def render

  def repage

  def samplingFactor

  def scene

  def seed

  def strech

  def stroke

  def strokewidth

  def style

  def texture

  def title

  def transparentColor

  def treedepth

  def imagetype

  def undercolor

  def units

  //def verbose

  def virtualPixel

  def weight
*/

  object Compression extends Enumeration {
    type Compression = Value

    val B44, B44A, BZip, DXT1, DXT3, DXT5, Fax, Group4, JPEG, JPEG2000, Lossless, LosslessJPEG, LZW = Value
    val Piz, Pxr24, RLE, Zip, RunlengthEncoded, ZipS = Value
  }

object Channel extends Enumeration {
  type Channel = Value

  val All, Red, Green, Blue, Alpha, Cyan, Magenta, Yellow, Black, Opacity, Index, RGB, RGBA, CMYK, CMYKA = Value
}

/**
 * Used by #alpha function to set a flag on an image indicating whether or not to use existing alpha channel data,
 * to create an alpha channel, or to perform other operations on the alpha channel.
 */
object Alpha extends Enumeration {
  type Alpha = Value

  /**
   * Enable the image's transparency channel. Note normally Set should be used instead of this,
   * unless you specifically need to preserve existing (but specifically turned Off) transparency channel.
   */
  val Activate, On = Value

  /**
   * Disables the image's transparency channel. Does not delete or change the existing data,
   * just turns off the use of that data.
   */
  val Deactivate, Off = Value

  /**
   * Activates the alpha/matte channel. If it was previously turned off then it also resets the channel to opaque.
   * If the image already had the alpha channel turned on, it will have no effect.
   */
  val Set = Value

  /**
   * Enables the alpha/matte channel and forces it to be fully opaque.
   */
  val Opaque = Value

  /**
   * Activates the alpha/matte channel and forces it to be fully transparent.
   * This effectively creates a fully transparent image the same size as the original and with all its original
   * RGB data still intact, but fully transparent.
   */
  val Transparent = Value

  /**
   * Copies the alpha channel values into all the color channels and turns 'Off' the the image's transparency,
   * so as to generate a gray-scale mask of the image's shape. The alpha channel data is left intact just deactivated.
   * This is the inverse of 'Copy'.
   */
  val Extract = Value

  /**
   * Turns 'On' the alpha/matte channel, then copies the gray-scale intensity of the image, into the alpha channel,
   * converting a gray-scale mask into a transparent shaped mask ready to be colored appropriately.
   * The color channels are not modified.
   */
  val Copy = Value

  /**
   * As per 'Copy' but also colors the resulting shape mask with the current background color.
   * That is the RGB color channels is replaced, with appropriate alpha shape.
   */
  val Shape = Value

  /**
   * Set any fully-transparent pixel to the background color, while leaving it fully-transparent.
   * This can make some image file formats, such as PNG,
   * smaller as the RGB values of transparent pixels are more uniform, and thus can compress better.
   */
  val Background = Value
}

case class ImageSourceWithAttributes(override val imageSource: ImageSource,
                                     override val attributes: Iterable[ImageAttibute])
  extends ImageSource
  with ImageAttributes {

  override def writeTo(out: OutputStream) = imageSource.writeTo(out)

  def sourceSpec = imageSource.sourceSpec
}

class ImageAttibute(val name: String, val activate: Boolean = true) extends HasCommands {
  def commands = ((if (activate) "-" else "+") + name) :: Nil
}

