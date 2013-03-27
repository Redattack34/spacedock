package data.general

import data.xml.Localization._
import data.xml.Weapon._
import data.xml.Module._
import java.io.File
import javax.swing.Icon
import data.xnb.XnbReader
import javax.swing.JDialog
import javax.swing.JOptionPane
import javax.swing.ImageIcon
import data.xml.Hull._
import data.xml.Ship._
import com.google.common.base.Stopwatch
import java.util.concurrent.TimeUnit
import java.net.URL
import java.util.Arrays
import gui.ShipModel

class DataModel {
  
  private val install = Config.install
  private val user = Config.user

  private val englishFile = new File(install.getAbsolutePath() + "/Content/Localization/English.xml")
  
  private val hullsByRace : Map[String, Map[String, Hull]] = {
    val loadedHulls = showErrors( loadHulls(install) )
    loadedHulls.map(hull => (hull.name, hull)).toMap.groupBy(_._2.race)
  }
  private val tokens : Map[Int, String] = loadTokens(englishFile)
  private val weapons : Map[String, Weapon] = showErrors(loadWeapons(install)).map( weap => (weap.name, weap)).toMap
  private val modules : Map[String, ShipModule] = showErrors(loadModules(install)).map( mod => (mod.uid, mod)).toMap
  private val moduleImages : Map[String, ImageIcon] = loadModuleTextures
  private var shipDesigns : Map[String, Ship] = showErrors(loadShips(install, user)).map( ship => (ship.name, ship)).toMap

  val lightningBolt : ImageIcon = loadTexture( new File( install.getAbsolutePath() + "/Content/Textures/UI/lightningBolt.xnb" ) ).get

  private def loadTexture(f: File) : Option[ImageIcon] = {
    XnbReader.read(f) match {
      case Left(ex) => {
        JOptionPane.showMessageDialog(null, "Failed to load file: " + f + "\n" + ex,
            "SpaceDock: Failed to Load Texture", JOptionPane.ERROR_MESSAGE)
        None
      }
      case Right(im) => Some(new ImageIcon(im))
    }
  }
  
  def races = shipDesigns.map(_._2.race).toSet.toSeq.sorted
  
  def hulls(race: String) = hullsByRace(race).values.toSeq.sortBy(_.name)
  
  def ships(race: String, hull: String) = shipDesigns.values
      .filter(_.race == race)
      .filter(_.hull == hull)
      
  def hullForShip(s: Ship) = hullsByRace(s.race)(s.hull)
  
  def token(id: Int) = tokens(id)
  
  def weapon(weaponId: String) = weapons(weaponId)
  def weaponTypes = weapons.values.map(_.weaponType).toSet
  
  def module( moduleId: String ) = modules(moduleId)
  def shipModules = modules.values
  
  def moduleImage( mod: ShipModule) = moduleImages(mod.iconTexturePath)
  
  def loadShipFromFile( f: File ) : Option[Ship] = {
    val tupleOpt = loadShipsFromFile(f)
    tupleOpt.foreach( shipDesigns += _)
    tupleOpt.map(_._2)
  }

  def loadShipFromUrl( url: URL ) : Option[Ship] = {
    val tupleOpt = loadShipsFromUrl(url)
    tupleOpt.foreach( shipDesigns += _)
    tupleOpt.map(_._2)
  }

  def fighterDesigns : Array[String] =
    shipDesigns.values.filter( _.role == "fighter").map(_.name)
        .toSeq.sorted.toArray

  private def loadModuleTextures : Map[String, ImageIcon] = {
    val dir = new File( install.getAbsolutePath() + "/Content/Textures/Modules")
    val eithers = for { file <- dir.listFiles().par }
        yield ("Modules/" + file.getName().replace(".xnb", ""), loadTexture(file))

    val successes = eithers.seq.filter(_._2.isDefined).toMap
    successes.mapValues(_.get)
  }
  
  def save( ship: ShipModel ) : Ship = {
    val saved = saveShip(ship, user)
    shipDesigns += saved
    saved._2
  }

  private def showErrors[T]( values: Seq[(File, Option[T])]) : Seq[T] = {
    val failures = values.filter(_._2.isEmpty)
    
    if ( !failures.isEmpty ) {
      val errorString = failures.map(_._1).mkString("\n")
      JOptionPane.showMessageDialog(null, "Failed to read files: \n" + errorString,
          "Spacedock Error", JOptionPane.ERROR_MESSAGE);
    }
    
    values.collect{ case (_, Some(t)) => t }
  }
  
  private def time[T]( str: String, f: => T ) : T = {
    val watch = new Stopwatch
    watch.start
    val ret = f
    watch.stop
    println(str + " Elapsed Time: " + watch.elapsed(TimeUnit.MILLISECONDS))
    ret
  }
}