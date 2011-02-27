package image

import java.io.File
import org.mbr.imagery.image.Image

/**
 * User: mathias
 * Date: 27.02.11 14:18
 * Time: 14:18
 */

trait ImageScanner {

  def location: File

  def execute[A](f: Image => A): Iterable[A] = Nil
}