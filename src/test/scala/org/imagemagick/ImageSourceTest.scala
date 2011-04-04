package org.imagemagick

import org.specs.Specification
import java.io.File
import Convert._

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
      val (location :: Nil) = apply(file).arguments
      location must_== file.getAbsolutePath

      "frame range" in {
        val (filePathWithFrames :: Nil) = apply(source.frames(0 to 3)).arguments
        filePathWithFrames must endWith("fruehling.jpg[0-3]")
      }

      "frame index" in {
        val (filePathWithFrames :: Nil) = apply(source.frames(3, 2, 4)).arguments
        filePathWithFrames must endWith("fruehling.jpg[3,2,4]")
      }

      "size attribute" in {
        val (somesize :: widthHeight :: path :: Nil) = convert.size(55,66).apply(file).arguments
        somesize must_== "-size"
        widthHeight must_== "55x66"
        path must_== file.getAbsolutePath
      }

      "depth attribute" in {
        val (deptharg :: depthValue :: path :: Nil) = convert.depth(16).apply(file).arguments
        deptharg must_== "-depth"
        depthValue must_== "16"
        path must_== file.getAbsolutePath
      }

      "resized" in {
        val (path :: other) = apply(source.resized(width = 100, height = 200)).arguments
        path must endWith("fruehling.jpg[100x200]")
      }

      "cropped" in {
        val (path :: other) = apply(source.cropped(Geometry(120, 120).offset(10, 5))).arguments
        path must endWith("fruehling.jpg[120x120+10+5]")
      }
    }
  }
}