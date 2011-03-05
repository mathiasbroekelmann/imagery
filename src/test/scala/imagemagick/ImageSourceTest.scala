package org.imagemagick

import org.specs.Specification
import org.imagemagick._
import java.io.File

/**
 * User: mathias
 * Date: 05.03.11 15:44
 * Time: 15:44
 */

class ImageSourceTest extends Specification with ImageSourceSpec {
  "image source" should {
    "have a file image source" in {
      ImageSourceSpec.file(new File("src/test/resources/fruehling.jpg"))
    }
  }
}