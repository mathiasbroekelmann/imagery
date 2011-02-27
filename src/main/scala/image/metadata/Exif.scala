package image.metadata

import org.joda.time.DateTime
import com.drew.metadata.exif.ExifDirectory
import com.drew.metadata.{Directory, Metadata}

object Orientation extends Enumeration {
  type Orientation = Value

  val TopLeft = Value(1, "top, left side")
  val TopRight = Value(2, "top, right side")
  val BottomRight = Value(3, "bottom, right side")
  val BottomLeft = Value(4, "bottom, Left")
  val LeftTop = Value(5, "left side, top")
  val RightTop = Value(6, "right side, top")
  val RightBottom = Value(7, "right side, bottom")
  val LeftBottom = Value(8, "left side, bottom")
}

trait RichMetadata {

  type DATE = {
    def apply: Option[DateTime]
    def original: Option[DateTime]
    def digitized: Option[DateTime]
  }

  type DIMENSION = {
    def width: Int
    def height: Int
  }

  def make: Option[String]
  def model: Option[String]
  def width: Option[Int]
  def height: Option[Int]
  def orientation: Option[Orientation.Orientation]
  def date: DATE
  def dimension: Option[DIMENSION]
}

object Exif {

  implicit def richMetadata(meta: Metadata): RichMetadata = new RichMetadata {

    lazy val exif = meta.getDirectory(classOf[ExifDirectory])

    def make = stringValue(exif, ExifDirectory.TAG_MAKE)

    def model = stringValue(exif, ExifDirectory.TAG_MODEL)

    def width = intValue(exif, ExifDirectory.TAG_EXIF_IMAGE_WIDTH)
    def height = intValue(exif, ExifDirectory.TAG_EXIF_IMAGE_HEIGHT)

    def orientation = intValue(exif, ExifDirectory.TAG_ORIENTATION) match {
      case Some(i) if i >= 1 && i <= 8 ⇒ Some(Orientation(i))
      case _ ⇒ None
    }

    def date = new {
      def apply = dateValue(exif, ExifDirectory.TAG_DATETIME)
      def original = dateValue(exif, ExifDirectory.TAG_DATETIME_ORIGINAL)
      def digitized = dateValue(exif, ExifDirectory.TAG_DATETIME_DIGITIZED)
    }

    def dimension =
      for (w ← width; h ← height) yield new {
        def width = w
        def height = h
      }

    def stringValue(dir: Directory, tag: Int): Option[String] =
      if (dir.containsTag(tag)) Some(dir.getString(tag)) else None

    def intValue(dir: Directory, tag: Int): Option[Int] =
      if (dir.containsTag(tag)) Some(dir.getInt(tag)) else None

    def dateValue(dir: Directory, tag: Int): Option[DateTime] =
      if (dir.containsTag(tag) && dir.getDate(tag) != null) Some(new DateTime(dir.getDate(tag))) else None

  }
}
