package data.xml

import java.io.File
import java.net.URL

import com.codecommit.antixml.Elem
import com.codecommit.antixml.XML

trait XmlLoader[T] {

  def load(f: Option[File], e : Elem) : Seq[T]
  
  def directory( base: File) : File
  
  def safeLoad(f: Option[File], e : Elem) : Seq[T] = {
    try {
      load(f, e)
    }
    catch {
      case ex: Exception => { ex.printStackTrace(); Seq() }
    }
  }
  
  def loadAll( base: File ) : Seq[(File, Option[T])] = {
    val dir = directory(base)
    
    if ( !dir.exists || !dir.canRead ) return Seq()
    
    loadFromDirectory(dir)
  }
  
  def loadFromDirectory( dir: File ) : Seq[(File, Option[T])] = {
    val allItems = for {
      file <- dir.listFiles().toSeq.par
      if (file.isFile())
      xml = XML.fromInputStream(XmlUtils.read(file))
      items = safeLoad(Some(file), xml)
    } yield (file, items.headOption)
    allItems.seq
  }
  
  def loadFromFile( f: File ) : Option[T] = {
    val xml = XML.fromInputStream(XmlUtils.read(f))
    safeLoad(Some(f), xml).headOption
  }
  
  def loadFromUrl( url: URL ) : Option[T] = {
    val xml = XML.fromInputStream(XmlUtils.read(url))
    safeLoad(None, xml).headOption
  }
}