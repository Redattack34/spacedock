package data.xml

import java.io.File

import scala.Option.option2Iterable

import com.codecommit.antixml.Elem
import com.codecommit.antixml.Selector.symbolToSelector
import com.codecommit.antixml.text

import data.general.FileExtension.extension2File
import data.general.FileExtension.file2Extension

case class Mod(name: String, desc: String, file: File) {
  def dir = file.getName().replace(".xml", "")
}

object Mod extends XmlLoader[Mod]{

  def load(f: Option[File], e: Elem) : Seq[Mod] = for {
    name <- e \ 'ModName \ text
    desc <- e \ 'ModDescription \ text
    file <- f
  } yield Mod(name, desc, file)
  
  def directory(base: File) = base / 'Mods
}