package data.xml

import java.io.File

import com.codecommit.antixml.Elem
import com.codecommit.antixml.Selector.symbolToSelector
import com.codecommit.antixml.text

import data.general.FileExtension.extension2File
import data.general.FileExtension.file2Extension


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

  def load( f: Option[File], e: Elem ) : Seq[Technology] = {
    val nameId = (e \ 'NameIndex \ text)
    val allModules = modules(e)
    val allHulls = hulls(e)
    val tech = Technology( nameId.headOption.map(_.toInt).getOrElse(509), allModules, allHulls)
    Seq(tech)
  }
  //509 is the name ID for the 'correct' Ancient Repulsor. This avoids failing
  //on the invalid one.
}