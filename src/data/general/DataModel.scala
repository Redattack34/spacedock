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
import scala.swing.Publisher
import scala.swing.event.Event
import scala.swing.Reactor
import gui.LoadMod
import gui.ClearMods
import gui.UnloadMod
import data.xml.Technology
import data.xml.Research
import com.weiglewilczek.slf4s.Logging
import java.awt.image.BufferedImage
import java.awt.image.RescaleOp
import java.awt.RenderingHints
import java.awt.Image

class ModuleImage( val img: BufferedImage ) {
  lazy val highlight : BufferedImage = {
    val op = new RescaleOp( Array[Float](2.0f, 1.0f, 1.0f, 1.0f),
        Array[Float](0.0f, 0.0f, 0.0f, 0.0f ), null.asInstanceOf[RenderingHints] )
    val dst = new BufferedImage( img.getWidth(), img.getHeight(), BufferedImage.TYPE_INT_ARGB);
    dst.getGraphics().drawImage( img, 0, 0, null )
    op.filter(img, dst)
    dst
  }

  lazy val icon = new ImageIcon( img )
}
class ModData(val name: String, val dir: File) extends Logging {
  import DataModel._

  logger.info("Loading " + name + " from " + dir.getAbsolutePath)

  private val englishFile = dir / 'Localization / "English.xml"

  val hullsByRace : Map[String, Map[String, Hull]] = {
    val loadedHulls = showErrors( Hull.loadAll(dir) )
    loadedHulls.map(hull => (hull.name, hull)).toMap.groupBy(_._2.race)
  }

  val tokens : Map[Int, String] = loadTokens(englishFile)
  val weapons : Map[String, Weapon] = showErrors(Weapon.loadAll(dir)).map( weap => (weap.name, weap)).toMap
  val modules : Map[String, ShipModule] = showErrors(Module.loadAll(dir)).map( mod => (mod.uid, mod)).toMap
  val moduleImages : Map[String, ModuleImage] = loadModuleTextures
  val shipDesigns : Map[String, Ship] = showErrors(Ship.loadAll(dir)).map( ship => (ship.name, ship)).toMap
  val technologies : Seq[Technology] = showErrors(Research.loadAll(dir))
  val techsByMod = technologies.flatMap(t => t.modules.map( (_, t) )).toMap
  val techsByHull = technologies.flatMap(t => t.hulls.map( (_, t) )).toMap

  private def loadModuleTextures : Map[String, ModuleImage] = {
    val dir = this.dir / 'Textures / 'Modules

    if ( !dir.exists() ) return Map()

    val eithers = for { file <- dir.listFiles().par }
        yield ("Modules/" + file.getName().replace(".xnb", ""), loadTexture(file))

    val successes = eithers.seq.filter(_._2.isDefined).toMap
    successes.mapValues(_.get)
  }
}

case object ReloadFromModel extends Event

class DataModel extends Publisher with Reactor {
  import DataModel._

  private val install = Config.install
  private val user = Config.user

  private val content = install / 'Content
  val baseGame = new ModData("StarDrive", content)

  val allMods : Map[String, Mod] = showErrors(Mod.loadAll(install)).map(mod => (mod.name, mod)).toMap

  var _modData = Config.mods
                .flatMap(mod => allMods.get(mod))
                .map(mod => (mod, install / 'Mods / mod.dir))
                .map{ case (mod, dir) => new ModData( mod.name, dir)}
  def modData = _modData
  def modData_=(data: Seq[ModData]) = {
    _modData = data
    allData = baseGame +: modData
    publish(ReloadFromModel)
  }

  var allData = baseGame +: modData

  var customShipDesigns = showErrors(Ship.loadCustomShips(user)).map( ship => (ship.name, ship)).toMap

  val lightningBolt : Image = loadTexture( install / 'Content / 'Textures / 'UI / "lightningBolt.xnb" ).get.img

  def hullsByRace  = allData.map(_.hullsByRace ).reduceLeft(_ ++ _)
  def tokens       = allData.map(_.tokens      ).reduceLeft(_ ++ _)
  def weapons      = allData.map(_.weapons     ).reduceLeft(_ ++ _)
  def modules      = allData.map(_.modules     ).reduceLeft(_ ++ _).filter(_._2.moduleType != "Dummy")
  def moduleImages = allData.map(_.moduleImages).reduceLeft(_ ++ _)
  def shipDesigns  = (allData.map(_.shipDesigns ).reduceLeft(_ ++ _) ++ customShipDesigns)
    .filter(_._2.requiredModsList.forall(loadedMods.contains))
  def techsByMod   = allData.map(_.techsByMod  ).reduceLeft(_ ++ _)
  def techsByHull  = allData.map(_.techsByHull ).reduceLeft(_ ++ _)

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
  def loadedMods : Seq[String] = modData.map(_.name)

  def techsForMods( mods: Iterable[ShipModule]) : Set[Technology] = {
    val techs = techsByMod
    mods.map(_.uid).flatMap(techs.get).toSet
  }

  def techForHull( race: String, hull: String ) : Option[Technology] =
    techsByHull.get( race + "/" + hull )

  def save( ship: ShipModel ) : Ship = {
    val saved = Ship.saveShip(ship, user)
    customShipDesigns += (saved.name -> saved)
    saved
  }

  def saveToFile( ship: ShipModel, file: File ) : Ship = {
    val saved = Ship.saveShipToFile(ship, file)
    customShipDesigns += (saved.name -> saved)
    saved
  }

  reactions += {
    case LoadMod(mod) => modData = modData :+ new ModData(mod.name, install / 'Mods / mod.dir)
    case UnloadMod(mod) => modData = modData.filter(_.name != mod.name)
    case ClearMods => modData = Seq()
  }
}

object DataModel extends Logging {

  def showErrors[T]( values: Seq[(File, Option[T])]) : Seq[T] = {
    val failures = values.filter(_._2.isEmpty)

    if ( !failures.isEmpty ) {

      failures.map(_._1).foreach( f => logger.error("Failed to load file: " + f.getAbsolutePath) )

      val errorString = failures.map(_._1).mkString("\n")
      JOptionPane.showMessageDialog(null, "Failed to read files: \n" + errorString,
          "Spacedock Error", JOptionPane.ERROR_MESSAGE);
    }

    values.collect{ case (_, Some(t)) => t }
  }


  def loadTexture(f: File) : Option[ModuleImage] = {
    XnbReader.read(f) match {
      case Left(ex) => {
        logger.error("Failed to load texture: " + f.getAbsolutePath, ex)
        JOptionPane.showMessageDialog(null, "Failed to load file: " + f + "\n" + ex,
            "SpaceDock: Failed to Load Texture", JOptionPane.ERROR_MESSAGE)
        None
      }
      case Right(im) => Some(new ModuleImage(im.asInstanceOf[BufferedImage]))
    }
  }
}