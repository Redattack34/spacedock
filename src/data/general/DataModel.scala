package data.general

import java.io.File
import java.net.URL
import java.util.concurrent.TimeUnit
import com.google.common.base.Stopwatch
import data.xml.Hull
import data.xml.Localization.loadTokens
import data.xml.Module
import data.xml.Mod
import data.xml.Ship
import data.xml.ShipModule
import data.xml.Weapon
import data.xnb.XnbReader
import gui.ShipModel
import javax.swing.ImageIcon
import javax.swing.JOptionPane
import data.xml.Mod
import data.general.FileExtension._

class ModData(dir: File) { 
  import DataModel._
  
  private val englishFile = dir / 'Localization / "English.xml"
	
  val hullsByRace : Map[String, Map[String, Hull]] = {
	val loadedHulls = showErrors( Hull.loadAll(dir) )
	loadedHulls.map(hull => (hull.name, hull)).toMap.groupBy(_._2.race)
  }
  val tokens : Map[Int, String] = loadTokens(englishFile)
  val weapons : Map[String, Weapon] = showErrors(Weapon.loadAll(dir)).map( weap => (weap.name, weap)).toMap
  val modules : Map[String, ShipModule] = showErrors(Module.loadAll(dir)).map( mod => (mod.uid, mod)).toMap
  val moduleImages : Map[String, ImageIcon] = loadModuleTextures
  val shipDesigns : Map[String, Ship] = showErrors(Ship.loadAll(dir)).map( ship => (ship.name, ship)).toMap
  
  private def loadModuleTextures : Map[String, ImageIcon] = {
    val dir = this.dir / 'Textures / 'Modules
    val eithers = for { file <- dir.listFiles().par }
        yield ("Modules/" + file.getName().replace(".xnb", ""), loadTexture(file))

    val successes = eithers.seq.filter(_._2.isDefined).toMap
    successes.mapValues(_.get)
  }
}

class DataModel {
  import DataModel._
  
  private val install = Config.install
  private val user = Config.user
  
  private val content = install / 'Content
  val baseGame = new ModData(content)
  
  val allMods : Map[String, Mod] = showErrors(Mod.loadAll(install)).map(mod => (mod.name, mod)).toMap
  
  val modData = Config.mods.flatMap(mod => allMods.get(mod)).map(mod => install / 'Mods / mod.name).map(new ModData(_))
  
  val allData = baseGame +: modData
  
  var customShipDesigns = showErrors(Ship.loadCustomShips(user)).map( ship => (ship.name, ship)).toMap

  val lightningBolt : ImageIcon = loadTexture( install / 'Content / 'Textures / 'UI / "lightningBolt.xnb" ).get
  
  private def hullsByRace  = allData.map(_.hullsByRace ).reduceLeft(_ ++ _)
  private def tokens       = allData.map(_.tokens      ).reduceLeft(_ ++ _)
  private def weapons      = allData.map(_.weapons     ).reduceLeft(_ ++ _)
  private def modules      = allData.map(_.modules     ).reduceLeft(_ ++ _)
  private def moduleImages = allData.map(_.moduleImages).reduceLeft(_ ++ _)
  private def shipDesigns  = allData.map(_.shipDesigns ).reduceLeft(_ ++ _) ++ customShipDesigns
      
  def races = hullsByRace.keys.toSeq.sorted
  
  def hulls(race: String) = hullsByRace(race).values.toSeq.sortBy(_.name)
  
  def ships(race: String, hull: String) = shipDesigns.values
      .filter(_.race == race)
      .filter(_.hull == hull)
      
  def hullForShip(s: Ship) = hullsByRace(s.race)(s.hull)
  
  def token(id: Int) = tokens(id)
  
  def weapon(weaponId: String) = weapons.get(weaponId).orElse(weapons.get(weaponId.replace("Dual", "").trim)).get
  def weaponTypes = weapons.values.map(_.weaponType).toSet
  
  def module( moduleId: String ) = modules(moduleId)
  def shipModules = modules.values
  
  def moduleImage( mod: ShipModule) = moduleImages(mod.iconTexturePath)
  
  def loadShipFromFile( f: File ) : Option[Ship] = {
    val ship = Ship.loadFromFile(f)
    ship.foreach( ship => customShipDesigns += (ship.name -> ship))
    ship
  }

  def loadShipFromUrl( url: URL ) : Option[Ship] = {
    val ship = Ship.loadFromUrl(url)
    ship.foreach( ship => customShipDesigns += (ship.name -> ship))
    ship
  }

  def fighterDesigns : Array[String] =
    shipDesigns.values.filter( _.role == "fighter").map(_.name)
        .toSeq.sorted.toArray
        
  def mods : Seq[Mod] = allMods.values.toSeq.sortBy(_.name)
  
  def save( ship: ShipModel ) : Ship = {
    val saved = Ship.saveShip(ship, user)
    customShipDesigns += (saved.name -> saved)
    saved
  }
}

object DataModel {
  
  def showErrors[T]( values: Seq[(File, Option[T])]) : Seq[T] = {
    val failures = values.filter(_._2.isEmpty)
    
    if ( !failures.isEmpty ) {
      val errorString = failures.map(_._1).mkString("\n")
      JOptionPane.showMessageDialog(null, "Failed to read files: \n" + errorString,
          "Spacedock Error", JOptionPane.ERROR_MESSAGE);
    }
    
    values.collect{ case (_, Some(t)) => t }
  }
    
    
  def loadTexture(f: File) : Option[ImageIcon] = {
    XnbReader.read(f) match {
      case Left(ex) => {
        JOptionPane.showMessageDialog(null, "Failed to load file: " + f + "\n" + ex,
            "SpaceDock: Failed to Load Texture", JOptionPane.ERROR_MESSAGE)
        None
      }
      case Right(im) => Some(new ImageIcon(im))
    }
  }
}