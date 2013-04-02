package data.xml

import java.io.File
import data.general.FileExtension._
import com.codecommit.antixml.Elem
import com.codecommit.antixml.Selector.symbolToSelector
import com.codecommit.antixml.XML
import com.codecommit.antixml.text


case class Technology( nameID: Int, modules: Seq[String], hulls: Seq[String] )
object Research extends XmlLoader[Technology] {

  def directory( base: File ) = base / 'Technology
  
  def modules( e: Elem ) : Seq[String] = for {
    modList <- e \ 'ModulesUnlocked
    modUnlocked <- modList \ 'UnlockedMod
    mod <- modUnlocked \ 'ModuleUID \ text
  } yield mod
  
  def hulls( e: Elem ) : Seq[String] = for {
    hullList <- e \ 'HullsUnlocked
    unlockedHull <- hullList \ 'UnlockedHull
    hull <- unlockedHull \ 'Name \ text
  } yield hull
  
  def load( f: Option[File], e: Elem ) : Seq[Technology] = for {
    nameId <- e \ 'NameIndex \ text
    allModules = modules(e)
    allHulls = hulls(e)
  } yield Technology ( nameId.toInt, allModules, allHulls)
}