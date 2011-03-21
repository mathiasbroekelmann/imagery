package org.imagemagick

import org.apache.commons.lang.builder.HashCodeBuilder
import java.io.{File, OutputStream}

/**
 * User: mathias
 * Date: 05.03.11 18:34
 * Time: 18:34
 */
object ImageSettings extends ImageSettings {

  type Settings = ImageSettings

  def apply(setting: ImageSetting) = DefaultImageSettings(setting :: Nil)

  def commands = Nil
}

case class DefaultImageSettings(commands: Iterable[HasCommands]) extends ImageSettings {

  def apply(setting: ImageSetting) = DefaultImageSettings(commands ++ (setting :: Nil))

  type Settings = ImageSettings
}

trait ImageSettings extends Commands {

  type Settings <: ImageSettings

  /**
   * apply the image setting the the list of existing commands.
   * function is used to create the result of the apply function.
   */
  def apply(setting: ImageSetting): Settings

  /**
   * set the width and height of the image.
   */
  def size(width: Int, height: Int, offset: Option[Int] = None) =
    apply(new ParameterImageAttribue("size", (width + "x" + height + offset.map("+" + _.toString).getOrElse(""))))

  /**
   * Join images into a single multi-image file.
   */
  def adjoin: Settings = adjoin()

  /**
   * Join images into a single multi-image file.
   * Use #singleFile = false to force each image to be written to separate files
   */
  def adjoin(singleFile: Boolean = true) = apply(new ImageAttibute("adjoin", singleFile))

  /**
   * Applies the given transformation matrix.
   */
  def affine = transform _

  /**
   * Applies the given transformation matrix.
   */
  def transform(sx: Double = 0, rx: Double = 0, ry: Double = 0, sy: Double = 0, tx: Double = 0, ty: Double = 0) =
    apply(new ParameterImageAttribue("affine", sx + "," + rx + "," + ry + "," + sy + "," + tx + "," + ty :: "-transform" :: Nil))

  /**
   * Gives control of the alpha/matte channel of an image.
   */
  def alpha(alphaType: Alpha.Alpha): Settings = alpha(alphaType.toString)

  /**
   * Gives control of the alpha/matte channel of an image.
   */
  def alpha(alphaType: String) = apply(new ParameterImageAttribue("alpha", alphaType.toString))

  /**
   * Enable of the rendering of anti-aliasing pixels when drawing fonts and lines.
   */
  def antialias: Settings = antialias(true)

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
  def authenticate(password: String) = apply(new ParameterImageAttribue("authenticate", password))

  /**
   * Set the background color.
   *
   * The color is specified using the format described under the fill option.
   * The default background color (if none is specified or found in the image) is white.
   */
  def background(color: Color) = apply(new ParameterImageAttribue("background", color.spec))

  // TODO: def bias

  /**
   * Use black point compensation.
   */
  def blackPointCompensation: Settings = blackPointCompensation(true)

  /**
   * Enable/Disable black point compensation.
   */
  def blackPointCompensation(activate: Boolean = true) = apply(new ImageAttibute("black-point-compensation", activate))

  /**
   * Set the blue chromaticity primary point.
   */
  def bluePrimary(x: Int, y: Int) = apply(new ParameterImageAttribue("blue-primary", x + "," + y))

  /**
   * Set the border color.
   *
   * The default border color is #DFDFDF, this shade of gray.
   */
  def borderColor(color: Color) = apply(new ParameterImageAttribue("bordercolor", color.spec))

  /**
   * The caption can contain special format characters listed in the Format and Print Image Properties.
   * These commands are expanded when the caption is finally assigned to the individual images.
   *
   * If the first character of string is @, the image caption is read from a file titled by the remaining
   * characters in the string. Comments read in from a file are literal; no embedded formatting characters are recognized.
   */
  def caption(caption: String) = apply(new ParameterImageAttribue("caption", caption))

  /**
   * Specify those image color channels to which subsequent operators are limited.
   */
  def channel(channel: Channel.Channel, moreChannels: Channel.Channel*) = {
    lazy val spec = channel :: Option(moreChannels).map(_.toList).getOrElse(Nil)
    apply(new ParameterImageAttribue("channel", spec.mkString(",")))
  }

  /**
   * Embed a comment in an image.
   */
  def comment(comment: String) = apply(new ParameterImageAttribue("comment", comment))

  /**
   * Use pixel compression specified by type when writing the image.
   */
  def compress(compressType: Compression.Compression): Settings = compress(Option(compressType))

  /**
   * Use pixel compression specified by type when writing the image.
   * Use None to store the binary image in an uncompressed format.
   */
  def compress(compressType: Option[Compression.Compression] = None) =
    apply(new ParameterImageAttribue("compress", compressType.map(_.toString), compressType.isDefined))

  /**
   * TODO: add specific global settings generally used to control coders and image processing operations.
   */
  def define(settings: Definitions): Settings = apply(settings)

  /**
   * display the next image after pausing.
   */
  def delay(ticks: Int, ticksPerSeconds: Int = 100, delay: Option[Delay.Delay] = None) =
    apply(new DelayAttibute(ticks, ticksPerSeconds, delay))

  /**
   * Set the horizontal and vertical resolution of an image for rendering to devices.
   */
  def density(width: Int, height: Int): Settings = density(width, Some(height))

  /**
   * Set the horizontal and optionally the vertical resolution of an image for rendering to devices.
   */
  def density(width: Int, height: Option[Int] = None) =
    apply(new ParameterImageAttribue("density", width + height.map("x" + _).getOrElse("")))

  def depth(value: Int) = {
    assert(value > 0, "Color depth must be greater than 0, but was " + value)
    apply(new ParameterImageAttribue("depth", value.toString))
  }

  /**
   * render text right-to-left or left-to-right.
   */
  def direction(dir: Direction.Direction) =
    apply(new ParameterImageAttribue("direction", dir.toString))

  /**
   * define the GIF disposal image setting for images that are being created or read in.
   *
   * TODO: implement +dispose
   */
  def dispose(method: Dispose.Dispose): Settings = this.dispose(method.toString)

  /**
   * define the GIF disposal image setting for images that are being created or read in.
   *
   * TODO: implement +dispose
   */
  def dispose(method: String) = apply(new ParameterImageAttribue("dispose", method))

  /**
   * Apply a Riemersma or Floyd-Steinberg error diffusion dither to images when general color reduction is
   * applied via an option, or automagically when saving to specific formats.
   * This enabled by default.
   */
  def dither(method: Dither.Dither): Settings = this.dither(method.toString)

  /**
   * Apply a Riemersma or Floyd-Steinberg error diffusion dither to images when general color reduction is
   * applied via an option, or automagically when saving to specific formats.
   * This enabled by default.
   */
  def dither(method: String) = apply(new ParameterImageAttribue("dither", method))

  /**
   * specify the text encoding.
   */
  def encoding(encoding: Encoding.Encoding): Settings = this.encoding(encoding.toString)

  /**
   * specify the text encoding.
   */
  def encoding(encoding: String) = apply(new ParameterImageAttribue("encoding", encoding))

  /**
   * Specify endianness (MSB or LSB) of the image.
   */
  def endian(endian: Endian.Endian) =
    apply(new ParameterImageAttribue("endian", endian.toString))

  /**
   * Extract the specified area from image.
   *
   * This option is most useful for extracting a subregion of a very large raw image
   */
  def extract(geometrie: ImageGeometry) =
    apply(new ParameterImageAttribue("extract", geometrie.spec))

  /**
   * Set a font family for text.
   */
  def family(fontFamily: String) =
    apply(new ParameterImageAttribue("family", fontFamily))

  /**
   * color to use when filling a graphic primitive.
   */
  def fill(color: Color) =
    apply(new ParameterImageAttribue("fill", color.spec))

  /**
   * Use this type of filter when resizing or distorting an image.
   * The result implements FilterType which allows the definition of expert settings.
   *
   * @see Filter for defining expert settings
   */
  def filter(filter: Filter) =
    apply(new ParameterImageAttribue("filter", filter.commands))

  /**
   * Use this type of filter when resizing or distorting an image.
   * The result implements FilterType which allows the definition of expert settings.
   *
   * @see FilterType for defining expert settings
   */
  def filter(filterType: FilterType.FilterType): Settings = filter(Filter(filterType))

  /**
   * set the font to use when annotating images with text, or creating labels.
   */
  def font(name: String) = apply(new ParameterImageAttribue("font", name))

  /**
   * the image format type.
   */
  def format(name: String) = apply(new ParameterImageAttribue("format", name))

  /**
   * Colors within this distance are considered equal.
   */
  def fuzz(distance: AbsoluteOrPercent) = apply(new ParameterImageAttribue("fuzz", distance.spec))

  /**
   * Colors within this distance are considered equal.
   */
  def fuzz(distance: Int): Settings = fuzz(Value(distance))

  /**
   * Set the preferred size and location of the image.
   */
  def geometry(geometry: ImageGeometry) = apply(new ParameterImageAttribue("geometry", geometry.spec))

  /**
   * Sets the current gravity suggestion for various other settings and options.
   */
  def gravity(value: Gravity.Gravity) = apply(new ParameterImageAttribue("gravity", value.toString))

  /**
   * Sets the current gravity suggestion for various other settings and options.
   */
  def greenPrimary(x: Int, y: Int) = apply(new ParameterImageAttribue("green-primary", x + "," + y))

  /**
   * the type of interlacing scheme.
   */
  def interlace(scheme: Interlace.Interlace) = apply(new ParameterImageAttribue("interlace", scheme.toString))

  /**
   * use this type of rendering intent when managing the image color.
   */
  def intent(intent: Intent.Intent) = apply(new ParameterImageAttribue("intent", intent.toString))

  /**
   * Set the pixel color interpolation method to use when looking up a color based on a floating point or real value.
   */
  def interpolate(interpolation: Interpolation.Interpolation) =
    apply(new ParameterImageAttribue("interpolate", interpolation.toString))

  /**
   * assign a label to an image.
   */
  def label = new {

    /**
     * reset any previous defined label.
     */
    def reset = ImageSettings.this.apply(new ImageAttibute("label", false))

    /**
     * assign a label to an image.
     */
    def apply(name: String) = ImageSettings.this.apply(new ParameterImageAttribue("label", name))
  }

  /**
   * Set the pixel cache resource limit. define the limit in bytes or number of files
   * use SizeConversions to define sizes like 2 GB
   */
  def limit(resource: CacheResource.CacheResource, limit: Long) =
    apply(new ParameterImageAttribue("limit", resource.toString :: limit.toString :: Nil))


  //TODO: def linewidth - missing doc

  /**
   * add Netscape loop extension to your GIF animation.
   */
  def loop(iterations: Int) = apply(new ParameterImageAttribue("loop", iterations.toString))

  /**
   * define an image mask
   */
  def mask = new {

    /**
     * reset any previous defined mask
     */
    def reset = ImageSettings.this.apply(new ImageAttibute("mask", false))

    /**
     * Composite the image pixels as defined by the mask.
     */
    def apply(file: File) = ImageSettings.this.apply(new ParameterImageAttribue("mask", file.getAbsolutePath))
  }

  /**
   * Specify the color to be used with the -frame option.
   */
  def mattcolor(color: Color) = apply(new ParameterImageAttribue("mattcolor", color.spec))

  /**
   * specify orientation of a digital camera image.
   */
  def orient(orientation: ImageOrientation.ImageOrientation) =
    apply(new ParameterImageAttribue("orient", orientation.toString))

  /**
   * Set the size and location of an image on the larger virtual canvas.
   */
  def page(geometry: ImageGeometry) = apply(new ParameterImageAttribue("page", geometry.spec))

  /**
   * For convenience you can specify the page size using media (see below).
   * Offsets can then be added as with other geometry arguments e.g. page(Letter offset (43, 43))
   */
  def page(media: Media) = apply(new ParameterImageAttribue("page", media.spec))

  /**
   * deactivate any previous page setting
   */
  def page = apply(new ImageAttibute("page", false))

  /**
   * pointsize of the PostScript, OPTION1, or TrueType font.
   */
  def pointsize(value: Int) = apply(new ParameterImageAttribue("pointsize", value.toString))

  /**
   * image preview type.
   * Use this option to affect the preview operation of an image
   */
  def preview(previewType: Preview.Preview) = apply(new ParameterImageAttribue("preview", previewType.toString))

  /**
   * JPEG/MIFF/PNG compression level. The value depends on the used image format.
   * See http://www.imagemagick.org/script/settings-line-options.php#quality
   */
  def quality(value: Int) = apply(new ParameterImageAttribue("quality", value.toString))

  /**
   * Set the red chromaticity primary point.
   */
  def redPrimary(x: Int, y: Int) = apply(new ParameterImageAttribue("red-primary", x + "," + y))

  /**
   * Set a region in which subsequent operations apply.
   * The x and y offsets are treated in the same manner as in crop
   */
  def region(geometry: ImageGeometry) = apply(new ParameterImageAttribue("region", geometry.spec))

  /**
   * render vector operations.
   * This useful when saving the result to vector formats such as MVG or SVG.
   */
  def render: Settings = render(true)

  /**
   * Enable/Disable render vector operations.
   * This useful when saving the result to vector formats such as MVG or SVG.
   */
  def render(activate: Boolean = true) = apply(new ImageAttibute("render", activate))

  /**
   * start defining repage setting
   */
  def repage = new {

    /**
     * completely remove/reset the virtual canvas meta-data from the images.
     */
    def reset = ImageSettings.this.apply(new ImageAttibute("repage", false))

    /**
     * Adjust the canvas and offset information of the image.
     */
    def apply(geometry: ImageGeometry) = ImageSettings.this.apply(new ParameterImageAttribue("repage", geometry.spec))

  }

  // TODO: sampling-factor

  /**
   * set scene number.
   *
   * This option sets the scene number of an image or the first image in an image sequence.
   */
  def scene(value: Int) = apply(new ParameterImageAttribue("scene", value.toString))

  /**
   * seed a new sequence of pseudo-random numbers
   */
  def seed = apply(new ImageAttibute("seed"))

  /**
   * Set a type of stretch style for fonts.
   * This setting suggests a type of stretch that ImageMagick should try to apply to the currently selected font family.
   */
  def strech(fontStretch: FontStretch.FontStretch) = apply(new ParameterImageAttribue("strech", fontStretch.toString))

  /**
   * color to use when stroking a graphic primitive.
   */
  def stroke(color: Color) = apply(new ParameterImageAttribue("stroke", color.spec))

  /**
   * set the stroke width.
   */
  def strokewidth(value: Int) = apply(new ParameterImageAttribue("strokewidth", value.toString))

  /**
   * Set a font style for text.
   */
  def style(fontStyle: FontStyle.FontStyle) = apply(new ParameterImageAttribue("style", fontStyle.toString))

  /**
   * name of texture to tile onto the image background.
   */
  def texture(file: File) = apply(new ParameterImageAttribue("texture", file.getAbsolutePath))

  /**
   * Set the transparent color.
   */
  def transparentColor(color: Color) = apply(new ParameterImageAttribue("transparent-color", color.spec))

  /**
   * tree depth for the color reduction algorithm.
   */
  def treedepth(value: Int) = apply(new ParameterImageAttribue("treedepth", value.toString))

  /**
   * the image type.
   */
  def imagetype(imageType: ImageType.ImageType) = apply(new ParameterImageAttribue("type", imageType.toString))

  /**
   * set the color of the annotation bounding box.
   */
  def undercolor(color: Color) = apply(new ParameterImageAttribue("undercolor", color.spec))

  /**
   * the units of image resolution.
   */
  def units(value: ImageResolutionUnit.ImageResolutionUnit) =
    apply(new ParameterImageAttribue("units", value.toString))

  /**
   * Specify contents of virtual pixels.
   */
  def virtualPixel(method: VirtualPixelMethod.VirtualPixelMethod) =
    apply(new ParameterImageAttribue("virtual-pixel", method.toString))

  /**
   * Set a font weight for text.
   */
  def weight(fontWeight: FontWeight.FontWeight) =
    apply(new ParameterImageAttribue("weight", fontWeight.toString))
}

object FontWeight extends Enumeration {
  type FontWeight = Value

  val All, Bold, Bolder, Lighter, Normal = Value
}


object VirtualPixelMethod extends Enumeration {
  type VirtualPixelMethod = Value

  val Background, Black, CheckerTile, Dither, Edge, Gray, HorizontalTile, HorizontalTileEdge, Mirror = Value
  val Random, Tile, Transparent, VerticalTile, VerticalTileEdge, White = Value
}

object ImageResolutionUnit extends Enumeration {
  type ImageResolutionUnit = Value

  val Undefined, PixelsPerInch, PixelsPerCentimeter = Value
}

object ImageType extends Enumeration {
  type ImageType = Value

  val Bilevel, Grayscale, GrayscaleMatte, Palette, PaletteMatte, TrueColor = Value
  val TrueColorMatte, ColorSeparation, ColorSeparationMatte = Value
}

object FontStyle extends Enumeration {
  type FontStyle = Value

  val Any, Italic, Normal, Oblique = Value
}

object FontStretch extends Enumeration {
  type FontStretch = Value

  val Any, Condensed, Expanded, ExtraCondensed, ExtraExpanded, Normal = Value
  val SemiCondensed, SemiExpanded, UltraCondensed, UltraExpanded = Value
}

object Preview extends Enumeration {
  type Preview = Value
  val AddNoise, Blur, Brightness, Charcoal, Despeckle, Dull, EdgeDetect, Gamma, Grayscale, Hue, Implode, JPEG = Value
  val OilPaint, Quantize, Raise, ReduceNoise, Roll, Rotate, Saturation, Segment, Shade, Sharpen, Shear, Solarize = Value
  val Spiff, Spread, Swirl, Threshold, Wave = Value
}

object ImageOrientation extends Enumeration {
  type ImageOrientation = Value
  val TopLeft, TopRight, BottomRight, BottomLeft, LeftTop, RightTop, RightBottom, LeftBottom = Value
}

object CacheResource extends Enumeration {
  type CacheResource = Value
  val File, Area, Memory, Map, Disk, Thread, Time = Value
}

object SizeConversions {
  implicit def size(value: Double) = SizeConversion(value)

  implicit def size(value: Int) = SizeConversion(value.toDouble)

  implicit def size(value: Long) = SizeConversion(value.toDouble)

  implicit def size(value: Float) = SizeConversion(value.toDouble)
}

case class SizeConversion(value: Double) {
  def KB = (value * 1024).toLong

  def kb = KB

  def MB = (value * 1024 * 1024).toLong

  def mb = MB

  def GB = (value * 1024 * 1024 * 1024).toLong

  def gb = GB
}

object Interpolation extends Enumeration {
  type Interpolation = Value
  val Average, Bicubic, Bilinear, filter, Integer, Mesh, NearestNeighbor, Spline = Value
}

object Intent extends Enumeration {
  type Intent = Value
  val Absolute, Perceptual, Relative, Saturation = Value
}

object Interlace extends Enumeration {
  type Interlace = Value
  val Line, None, Plane, Partition, GIF, JPEG, PNG = Value
}

trait AbsoluteOrPercent extends Parameter {
  def percent: Boolean
}

case class Percent(value: Double) extends AbsoluteOrPercent {
  def percent = true

  lazy val spec = value + "%"
}

case class Value(value: Int) extends AbsoluteOrPercent {
  def percent = false

  def spec = value.toString
}

object Endian extends Enumeration {
  type Endian = Value

  val LSB, MSB = Value
}

object Encoding extends Enumeration {
  type Encoding = Value

  val AdobeCustom, AdobeExpert, AdobeStandard, AppleRoman, BIG5, GB2312 = Value
  val None, SJIScode, Symbol, Unicode, Wansung = Value
  val Latin2 = Value("Latin 2")
}

object Dither extends Enumeration {
  type Dither = Value

  val None, FlyedSteinberg, Riemersma = Value
}

object Dispose extends Enumeration {
  type Dispose = Value

  /**
   * No disposal specified (equivalent to 'none').
   */
  val Undefined = Value

  /**
   * Do not dispose, just overlay next frame image.
   */
  val None = Value

  /**
   * Clear the frame area with the background color.
   */
  val Background = Value

  /**
   * Clear to the image prior to this frames overlay.
   */
  val Previous = Value
}

object Direction extends Enumeration {
  type Direction = Value

  val rightToLeft, rtl = Value("right-to-left")
  val leftToRight, ltr = Value("left-to-right")
}

object Delay extends Enumeration {
  type Delay = Value

  val < = Value("<")
  val > = Value(">")
}

class DelayAttibute(ticks: Int,
                    ticksPerSeconds: Int = 100,
                    condition: Option[Delay.Delay] = None)
  extends ImageAttibute("delay") {

  override def commands = {
    val cond = condition match {
      case Some(c) => c.toString
      case _ => ""
    }
    super.commands ::: ticks + (if (ticksPerSeconds == 100) "" else "x" + ticksPerSeconds) + cond :: Nil
  }
}

/**
 * TODO
 */
trait Definitions extends ImageSetting {

  /**
   * apply the image setting the the list of existing commands.
   * function is used to create the result of the apply function.
   */
  def apply(setting: ImageSetting): Definitions

  /**
   * remove all existing definitions
   */
  //def none: Definitions

  /**
   * Set the display range to the minimum and maximum pixel values for the DCM image format.
   */
  def dcmDisplayRangeReset: Definitions = this

  /**
   * Set the specify the layout engine for the DOT image format (e.g. neato).
   */
  def dotLayoutEngine(value: String): Definitions = this

  /**
   * Restrict the maximum JPEG file size, for example 400000 bytes.
   */
  def jpegExtend(bytes: Int): Definitions = this

  /**
   * Set the size hint of a JPEG image, for example, 128x128.
   * It is most useful for increasing performance and reducing the memory requirements when reducing the size of a
   * large JPEG image.
   */
  def jpegSize(geometry: ImageGeometry): Definitions = apply(new ParameterImageAttribue("define", "jpeg:size=" + geometry.spec))

  /**
   * Specify the compression factor to use while writing JPEG-2000 files.
   * The compression factor is the reciprocal of the compression ratio.
   * The valid range is 0.0 to 1.0, with 1.0 indicating lossless compression.
   * If defined, this value overrides the quality setting.
   * A quality setting of 75 results in a rate value of 0.06641.
   */
  def jp2Rate(value: Double): Definitions = this

  /**
   * turn playback caching off for streaming MNG.
   */
  def mngNeedCacheoff: Definitions = this

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
  def pngBitDepth(depth: Int): Definitions = this


}

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

trait ImageSetting extends HasCommands

class ImageAttibute(val name: String, val activate: Boolean = true) extends ImageSetting {
  def commands = ((if (activate) "-" else "+") + name) :: Nil
}

class ParameterImageAttribue(name: String,
                             param: => Iterable[String],
                             activate: Boolean = true)
  extends ImageAttibute(name, activate) {

  def this(name: String, param: => String) = this (name, param :: Nil)

  override def commands = super.commands ::: param.toList
}

