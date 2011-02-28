package image

import java.awt.Image
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.awt.Graphics2D
import java.awt.AlphaComposite

/**
 * User: mathias
 * Date: 28.02.11 22:46
 * Time: 22:46
 */

object ImageResizer {

  def resize(is: java.io.InputStream, maxWidth: Option[Int], maxHeight: Option[Int]): BufferedImage = {
    val originalImage: BufferedImage = ImageIO.read(is)

    val height = originalImage.getHeight
    val width = originalImage.getWidth

    if (maxWidth.isEmpty || maxHeight.isEmpty || maxWidth.exists(_ > width) || maxHeight.exists(_ > height))
      originalImage
    else {
      var scaledWidth: Int = width
      var scaledHeight: Int = height
      val ratio: Double = width / height
      if (maxWidth.exists(scaledWidth > _)) {
        scaledWidth = maxWidth.get
        scaledHeight = (scaledWidth.doubleValue / ratio).intValue
      }
      if (maxHeight.exists(scaledHeight > _)) {
        scaledHeight = maxHeight.get
        scaledWidth = (scaledHeight.doubleValue * ratio).intValue
      }
      val scaledBI = new BufferedImage(scaledWidth, scaledHeight, BufferedImage.TYPE_INT_RGB)
      val g = scaledBI.createGraphics
      g.setComposite(AlphaComposite.Src)
      g.drawImage(originalImage, 0, 0, scaledWidth, scaledHeight, null);
      g.dispose
      scaledBI
    }
  }
}