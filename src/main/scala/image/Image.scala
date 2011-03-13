package org.mbr.imagery.image

import org.mbr.imagery.blob._
import scala.None
import com.drew.imaging.ImageMetadataReader
import image.metadata.Exif._
import java.io.{BufferedInputStream, File}

trait Image {
  def blob: Blob

  lazy val dimension: Option[Dimension] = blob.read {
    in =>
      for (dim <- ImageMetadataReader.readMetadata(new BufferedInputStream(in)).dimension) yield
        Dimension(dim.width, dim.height)
  }.getOrElse(None)
}

case class Dimension(val width: Int,
                     val height: Int)
