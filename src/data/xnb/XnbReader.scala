package data.xnb

import java.io.ByteArrayInputStream
import java.io.File
import java.io.IOException
import java.io.InputStream
import java.lang.ProcessBuilder
import java.lang.String
import com.google.common.io.ByteStreams
import ImageFormat.ImageFormat
import InputStreamUtils.read7BitInt
import InputStreamUtils.readInt
import InputStreamUtils.readString
import java.awt.Image
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.io.FileOutputStream
import javax.imageio.stream.FileImageOutputStream
import java.util.Arrays
import javax.swing.JDialog
import javax.swing.JOptionPane
import javax.swing.ImageIcon

object XnbReader {

  case class ReaderType(val reader: String, val version: Int)
  case class XNBHeader(val platform: Char, val version: Byte, val flag: Byte, val size: Int, val readers: Seq[ReaderType], val sharedResourceCount: Int)
  case class TextureHeader(val typeIndex: Int, val format: ImageFormat, val width: Int, val height: Int, val mips: Int)

  //TODO: Write an XNB decompressor in Scala and get rid of this stupid hack.
  private def open(f: File) : Either[IOException, InputStream] = {
    if ( !f.exists() || !f.canRead() ) return Left(new IOException("Cannot read file: " + f.getAbsolutePath))

    try {
      val process = new ProcessBuilder("XNBDecompressor.exe", f.getAbsolutePath).start()

      val is = process.getInputStream
      val bytes = ByteStreams.toByteArray(is)

      val exitCode = process.waitFor

      exitCode match {
        case 0 => Right(new ByteArrayInputStream(bytes))
        case 1 => Left(new IOException("Damaged or invalid XNB file: " + f.getAbsolutePath ) )
        case 2 => Left(new IOException("File created with unsupported version of XNA: " + f.getAbsolutePath ) )
        case _ => Left(new IOException("Failed to read file (Unknown error): " + f.getAbsolutePath))
      }
    }
    catch {
      case ex: IOException => new Left(ex)
    }
  }

  private def parseHeader(is: InputStream) : Either[IOException, XNBHeader] = {
    val magic1 = is.read
    val magic2 = is.read
    val magic3 = is.read

    if ( magic1 != 'X' || magic2 != 'N' || magic3 != 'B' )
      return Left(new IOException("Damaged or invalid XNB file"))

    val platform = is.read.toChar
    val version = is.read.toByte
    val flag = is.read.toByte
    val size = readInt(is)
    val readerCount = read7BitInt(is)
    var readers = Seq[ReaderType]()

    for( i <- 0 until readerCount ) {
      val name = readString(is)
      val version = readInt(is)
      readers = ReaderType(name, version) +: readers
    }

    val sharedResourceCount = read7BitInt(is)

    Right(XNBHeader(platform, version, flag, size, readers.reverse, sharedResourceCount))
  }

  private def parseGraphics(header: XNBHeader, is: InputStream) : Either[IOException, TextureHeader] = {
    val typeId = read7BitInt(is)
    val format = readInt(is)
    val formatEnum = ImageFormat.getImageFormat(header.version, format)
    val width = readInt(is)
    val height = readInt(is)
    val mips = readInt(is)
    Right( TextureHeader(typeId, formatEnum, width, height, mips ) )
  }

  private def getImage( header: TextureHeader, is: InputStream ) : Either[IOException, Image] = {
    try {
      val dataSize = readInt(is)
      val image = header.format.toImg(is, header.width, header.height, header.mips);
      return Right(image)
    }
    catch {
      case ex: IOException => Left(ex)
    }
  }

  def read( f: File ) : Either[IOException, Image] = {
    val either = for ( stream <- open(f).right;
      header <- parseHeader(stream).right;
      graphicsHeader <- parseGraphics(header, stream).right;
      image <- getImage(graphicsHeader, stream).right )
        yield image

    either.left.map(ex => new IOException( "Failed to read texture: " + f.getAbsolutePath(), ex))
  }

}