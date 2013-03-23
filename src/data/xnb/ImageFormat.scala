package data.xnb

import java.awt.Image
import java.io.InputStream
import java.io.IOException

object ImageFormat {

  sealed trait ImageFormat {
    def toImg( is : InputStream, width: Int, height: Int, mips: Int ) : Image
  }

  case object Unknown extends ImageFormat {
    def toImg(is: InputStream, width: Int, height: Int, mips: Int ) = throw new IOException("Unknown image format");
  }

  case object Color extends ImageFormat {
    def toImg(is: InputStream, width: Int, height: Int, mips: Int ) =
      ColorReader.read(is, width, height)
  }

  case object Dxt1 extends ImageFormat {
    def toImg(is: InputStream, width: Int, height: Int, mips: Int ) =
      DxtUtil.DecompressDxt1(is, width, height)
  }

  case object Dxt5 extends ImageFormat {
    def toImg(is: InputStream, width: Int, height: Int, mips: Int ) =
      DxtUtil.DecompressDxt5(is, width, height)
  }

  def getImageFormat( version: Int, i: Int ) = version match {
    case 4 => getImageFormatLegacy( i )
    case 5 => getImageFormatNew( i )
  }

  def getImageFormatNew( i : Int ) = i match {
    case 0 => Color
    // case 1 => Bgr565
    // case 2 => Bgra5551
    // case 3 => Bgra4444
    case 4 => Dxt1
    // case 5 => Dxt3
    case 6 => Dxt5
    // case 7 => NormalizedByte2
    // case 8 => NormalizedByte4
    // case 9 => Rgba1010102
    // case 10 => Rg32
    // case 11 => Rgba64
    // case 12 => Alpha8
    // case 13 => Single
    // case 14 => Vector2
    // case 15 => Vector4
    // case 16 => HalfSingle
    // case 17 => HalfVector2
    // case 18 => HalfVector4
    // case 19 => HdrBlendable
    case _ => {
      System.err.println("Unknown new image format: " + i);
      Unknown
    }
  }

  def getImageFormatLegacy( i : Int ) = i match {
    case 1 => Color
    // case 2 => Bgr32
    // case 3 => Bgra1010102
    // case 4 => Rgba32
    // case 5 => Rgb32
    // case 6 => Rgba1010102
    // case 7 => Rg32
    // case 8 => Rgba64
    // case 9 => Bgr565
    // case 10 => Bgra5551
    // case 11 => Bgr555
    // case 12 => Bgra4444
    // case 13 => Bgr444
    // case 14 => Bgra2338
    // case 15 => Alpha8
    // case 16 => Bgr233
    // case 17 => Bgr24
    // case 18 => NormalizedByte2
    // case 19 => NormalizedByte4
    // case 20 => NormalizedShort2
    // case 21 => NormalizedShort4
    // case 22 => Single
    // case 23 => Vector2
    // case 24 => Vector4
    // case 25 => HalfSingle
    // case 26 => HalfVector2
    // case 27 => HalfVector4
    case 28 => Dxt1
    // case 29 => Dxt2
    // case 30 => Dxt3
    // case 31 => Dxt4
    case 32 => Dxt5
    // case 33 => Luminance8
    // case 34 => Luminance16
    // case 35 => LuminanceAlpha8
    // case 36 => LuminanceAlpha16
    // case 37 => Palette8
    // case 38 => PaletteAlpha16
    // case 39 => NormalizedLuminance16
    // case 40 => NormalizedLuminance32
    // case 41 => NormalizedAlpha1010102
    // case 42 => NormalizedByte2Computed
    // case 43 => VideoYuYv
    // case 44 => VideoUyVy
    // case 45 => VideoGrGb
    // case 46 => VideoRgBg
    // case 47 => Multi2Bgra32
    // case 48 => Depth24Stencil8
    // case 49 => Depth24Stencil8Single
    // case 50 => Depth24Stencil4
    // case 51 => Depth24
    // case 52 => Depth32
    // case 54 => Depth16
    // case 56 => Depth15Stencil1
    case _ => {
      System.err.println("Unknown legacy image format: " + i);
      Unknown
    }
  }

}