package org.imagemagick

import org.specs.Specification
import org.imagemagick.ImageSourceSpec._
import java.io.File

/**
 * User: mathias
 * Date: 05.03.11 15:44
 * Time: 15:44
 */

class ImageSourceTest extends Specification {
  "image source" should {
    "have a file image source" in {
      val file = new File("src/test/resources/fruehling.jpg")
      val source = image(file)

      source must notBeNull
      val (location :: Nil) = source.commands
      location must_== file.getAbsolutePath

      "frame range" in {
        val (filePathWithFrames :: Nil) = source.frames(0 to 3).commands
        filePathWithFrames must endWith("fruehling.jpg[0-3]")
      }

      "frame index" in {
        val (filePathWithFrames :: Nil) = source.frames(3, 2, 4).commands
        filePathWithFrames must endWith("fruehling.jpg[3,2,4]")
      }

      "size attribute" in {
        val (size :: widthHeight :: path :: Nil) = source.size(55, 66).commands
        size must_== "-size"
        widthHeight must_== "55x66"
        path must_== file.getAbsolutePath
      }

      "depth attribute" in {
        val (depth :: depthValue :: path :: Nil) = source.depth(16).commands
        depth must_== "-depth"
        depthValue must_== "16"
        path must_== file.getAbsolutePath
      }

      "resized" in {
        val (path :: Nil) = source.resized(width = 100, height = 200).commands
        path must endWith("fruehling.jpg[100x200]")
      }

      "cropped" in {
        val (path :: Nil) = source.cropped(Geometry(120, 120).offset(10, 5)).commands
        path must endWith("fruehling.jpg[120x120+10+5]")
      }
    }
  }
}