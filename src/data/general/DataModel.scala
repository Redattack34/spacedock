package data.general

import java.io.File
import java.net.URL
import java.util.concurrent.TimeUnit
import com.google.common.base.Stopwatch
import data.xml.Hull
import data.xml.Hull.loadHulls
import data.xml.Localization.loadTokens
import data.xml.Module.loadModules
import data.xml.Mod.loadMods
import data.xml.Ship
import data.xml.Ship.loadShips
import data.xml.Ship.loadShipsFromFile
import data.xml.Ship.loadShipsFromUrl
import data.xml.Ship.saveShip
import data.xml.ShipModule
import data.xml.Weapon
import data.xml.Weapon.loadWeapons
import data.xnb.XnbReader
import gui.ShipModel
import javax.swing.ImageIcon
import javax.swing.JOptionPane
import data.xml.Mod

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
  private val allMods : Map[String, Mod] = showErrors(loadMods(install)).map(mod => (mod.name, mod)).toMap

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
        
  def mods : Seq[Mod] = allMods.values.toSeq.sortBy(_.name)

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