package image

import java.net.URL
import java.security.MessageDigest
import java.io.{BufferedOutputStream, FileOutputStream, File, OutputStream}
import org.mbr.imagery.blob.{FileBlob, Blob}

/**
 * User: mathias
 * Date: 20.03.11 22:06
 * Time: 22:06
 */

trait ResourceCache {

  def cached[A](source: URL, id: String)(compute: OutputStream => A): Blob
}

trait ResourceComputation extends AnyRef {
  def apply(out: OutputStream): Unit
}

object ResourceCache extends ResourceCache {

  lazy val cacheDir = new File(System.getProperty("java.io.tmpdir"), "resourceCache")

  def cached[A](source: URL, id: String)(compute: OutputStream => A) =
    new FileResourceCache(cacheDir).cached(source, id)(compute)
}

class FileResourceCache(val baseDir: File) extends ResourceCache {

  def cached[A](source: URL, id: String)(compute: OutputStream => A) = {
    lazy val md5: String = {
      val digest = MessageDigest.getInstance("MD5")
      digest.reset
      def encode(b: Byte) = java.lang.Integer.toString(b & 0xff, 36)
      ("" /: digest.digest(source.toString.getBytes("UTF-8"))) (_+ encode(_))
    }

    def mayUse(cacheFile: File): Boolean = {
      cacheFile.exists && cacheFile.lastModified > source.openConnection.getLastModified      
    }

    // use a 2 level hierarchy to reduce file count in a directory
    val cachedir = new File(new File(baseDir, md5.take(2)), md5.drop(2).take(2))
    
    val cachedFile = new File(cachedir, md5 + "-" + id)
    if(!mayUse(cachedFile)) {
      cachedir.mkdirs
      val output = new BufferedOutputStream(new FileOutputStream(cachedFile))
      try {
        compute(output)
      } finally {
        output.close
      }
    }
    new FileBlob {
      def file = cachedFile
    }
  }
}