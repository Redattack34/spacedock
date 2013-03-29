package data.xnb

import java.awt.Image
import java.awt.image.BufferedImage
import java.io.InputStream

object ColorReader {

  def read(is: InputStream, width: Int, height: Int) : Image = {
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)

    for ( y <- 0 until height; x <- 0 until width ) {
      image.setRGB(x, y, InputStreamUtils.readInt(is))
    }

    image
  }
}