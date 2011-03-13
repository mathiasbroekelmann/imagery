package org.imagemagick

import org.specs.Specification
import Convert._
import java.io.{File, FileOutputStream, FileInputStream}

/**
 * User: mathias
 * Date: 12.03.11 21:01
 * Time: 21:01
 */

class ImageWriterSpec extends Specification {
  "image" should {
    val source = image("src/test/resources/fruehling.jpg")
    val operation = convert(source).thumbnail(Geometry(100, 100))


    "be written to some file location" in {
      val outFile = new File("target/fruehling.png")
      outFile.delete
      outFile mustNot exist
      operation.write.to("target/fruehling.png")
      outFile must exist
    }

    "be read from an inputstream and written to a stream" in {
      val outFile = new File("target/stream.png")
      outFile.delete
      outFile mustNot exist
      val stream = image(new FileInputStream("src/test/resources/fruehling.jpg")).buffered 
      convert(stream) thumbnail(Geometry(100, 100)) writeAs("png") to(new FileOutputStream("target/stream.png"))
      outFile must exist
    }
  }
}