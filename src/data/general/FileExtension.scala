package data.general

import java.io.File
import com.google.common.io.Files

class FileExtension( val file: File,  val path: Seq[String] ) {
  def /(str: String) : FileExtension = {
    new FileExtension( file, path :+ str)
  }
  def /(sym: Symbol) : FileExtension =  this / sym.name

  def extension : String = Files.getFileExtension(file.getName).toLowerCase
}
object FileExtension {
  implicit def file2Extension(file: File) = new FileExtension(file, Vector())
  implicit def extension2File(fileExtension: FileExtension) = {
    new File( fileExtension.file.getAbsolutePath() + File.separator + fileExtension.path.mkString(File.separator))
  }
}