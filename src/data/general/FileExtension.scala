package data.general

import java.io.File
import scala.collection._

class FileExtension( val file: File,  val path: immutable.Seq[String] ) {
  def /(str: String) : FileExtension = {
    new FileExtension( file, path :+ str)
  }
  def /(sym: Symbol) : FileExtension =  this / sym.name
}
object FileExtension {
  implicit def file2Extension(file: File) = new FileExtension(file, Vector())
  implicit def extension2File(fileExtension: FileExtension) = {
    new File( fileExtension.file.getAbsolutePath() + File.separator + fileExtension.path.mkString(File.separator))
  }
}