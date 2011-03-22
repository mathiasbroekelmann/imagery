package org.mbr.imagery.image

import org.mbr.imagery.blob._
import scala.None
import com.drew.imaging.ImageMetadataReader
import image.metadata.Exif._
import java.io.{BufferedInputStream, File}

trait Image extends Blob {

  /**
   * @return Some dimension of the image or None if dimension could not be determined.
   */
  lazy val dimension: Option[Dimension] = read {
    in =>
      for (dim <- ImageMetadataReader.readMetadata(new BufferedInputStream(in)).dimension) yield
        Dimension(dim.width, dim.height)
  }.getOrElse(None)

  def width = dimension map(_.width)
  def height = dimension map(_.height)
}

case class Dimension(width: Int, height: Int)
