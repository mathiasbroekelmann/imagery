package org.mbr.vaadin

import java.io.File

/**
 * User: mathias
 * Date: 25.03.11 23:59
 * Time: 23:59
 */

object Filesystem {
  implicit def richFile(file: File) = new RichFile(file)
}

class RichFile(file: File) {

  /**
   * Builds a new collection by applying a partial function to all sub files of this directory on which the function is defined
   * if this is not a directory an empty list is returned.
   */
  def collect[A](pf: PartialFunction[File, A]): Traversable[A] = {
    if(file.isDirectory && file.canRead) {
      for(subFile <- Option(file.listFiles).getOrElse(Array.empty).toStream; if pf.isDefinedAt(subFile)) yield {
        pf(subFile)
      }
    } else {
      Nil
    }
  }
}