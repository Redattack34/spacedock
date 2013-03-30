package data.xml

import com.codecommit.antixml.Elem
import com.codecommit.antixml.Selector.symbolToSelector
import com.codecommit.antixml.XML
import com.codecommit.antixml.text
import java.io.File
import data.general.FileExtension._

case class Mod(name: String, desc: String)

object Mod extends XmlLoader[Mod]{

  def load( e: Elem ) : Seq[Mod] = for {
    name <- e \ 'ModName \ text
    desc <- e \ 'ModDescription \ text
  } yield Mod(name, desc)
  
  def directory(base: File) = base / 'Mods
}