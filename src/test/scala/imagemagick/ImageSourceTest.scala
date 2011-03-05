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

    }
  }
}