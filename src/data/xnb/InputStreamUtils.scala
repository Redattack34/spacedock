package data.xnb

import java.io.InputStream

import com.google.common.base.Charsets

object InputStreamUtils {
  def readInt(is: InputStream) : Int = {
    var i : Int = 0
    i = i | (is.read)
    i = i | (is.read << 8)
    i = i | (is.read << 16)
    i = i | (is.read.toLong << 24).toInt
    i
  }

  def readUInt(is: InputStream) : Long = {
    var i : Long = readInt(is)
    i &= 0x00000000FFFFFFFFL
    i
  }

  def read7BitInt(is: InputStream) : Int = {
    var result = 0
    var bitsRead = 0
    var value = 0

    do {
      value = is.read
      result |= (value & 0x7F) << bitsRead;
      bitsRead += 7;
    }
    while( (value & 0x80) != 0 )

    value
  }

  def readShort(is: InputStream) : Short = {
    var i : Short = 0;
    i = (i | (is.read)).toShort
    i = (i | (is.read << 8)).toShort
    i
  }

  def readUShort(is: InputStream ) : Int = {
    var i : Int = readShort(is)
    i &= 0xFFFF
    i
  }

  def read6Bytes(is: InputStream) : Long = {
    var i : Long = 0
    for ( x <- 0 until 6 ) {
      var value : Long = is.read();
      value &= 0xFF;
      i |= value << (x * 8)
    }
    i
  }

  def readString(is: InputStream) : String = {
    val length = read7BitInt(is)
    val buffer = new Array[Byte](length)
    is.read(buffer)
    new String(buffer, Charsets.UTF_8)
  }
}