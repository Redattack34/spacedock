package data.xml

import java.io.PushbackInputStream
import java.io.InputStream
import java.io.FileInputStream
import java.io.File
import java.net.URL
import java.io.BufferedInputStream
import java.nio.ByteBuffer

object XmlUtils {

  private def removeBOM(is: InputStream) : InputStream = {
    val pb = new PushbackInputStream(is, 3)

    val buffer = new Array[Byte](3)
    pb.read(buffer)
    val bytes = buffer.toList.map(_.toInt).map( _ & 0xFF )

    bytes match {
      case 0xEF :: 0xBB :: 0xBF :: rest => is
      case _ => {
        pb.unread(buffer)
        pb
      }
    }
  }

  def read( f: File ) : InputStream = removeBOM( new FileInputStream(f))
  def read( url: URL ) : InputStream = removeBOM( url.openStream() )
}