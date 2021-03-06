package data.xml

import java.io.File
import java.net.URL

import com.codecommit.antixml.Elem
import com.codecommit.antixml.XML
import com.weiglewilczek.slf4s.Logging

import scalaz.Scalaz._

trait XmlLoader[T] extends Logging {

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

  def openFile(file: File) = {
    logger.debug("Loading file: " + file)
    XML.fromInputStream(XmlUtils.read(file))
  }

  def loadFromDirectory( dir: File ) : Seq[(File, Option[T])] = {
    try {
      val allItems = for {
        file <- dir.listFiles().toSeq.par
        if (file.isFile())
        xml = openFile(file)
        items = safeLoad(file.some, xml)
      } yield (file, items.headOption)
      allItems.seq
    }
    catch {
      case ex => { logger.error(dir.toString); throw ex }
    }
  }

  def loadFromFile( f: File ) : Option[T] = {
    val xml = XML.fromInputStream(XmlUtils.read(f))
    safeLoad(f.some, xml).headOption
  }

  def loadFromUrl( url: URL ) : Option[T] = {
    val xml = XML.fromInputStream(XmlUtils.read(url))
    logger.debug("Loading from URL: " + url)
    safeLoad(None, xml).headOption
  }
}