package data.general

import java.awt.Image
import java.awt.RenderingHints
import java.awt.image.BufferedImage
import java.awt.image.RescaleOp
import java.io.File
import java.net.URL
import javax.swing.ImageIcon
import javax.swing.JOptionPane
import scala.Option.option2Iterable
import scala.swing.Publisher
import scala.swing.Reactor
import scala.swing.event.Event
import scalaz.Scalaz._
import com.weiglewilczek.slf4s.Logging
import data.general.FileExtension.extension2File
import data.general.FileExtension.file2Extension
import data.xml.Hull
import data.xml.Localization.loadTokens
import data.xml.Mod
import data.xml.Module
import data.xml.Research
import data.xml.Ship
import data.xml.ShipModule
import data.xml.Technology
import data.xml.Weapon
import data.xnb.XnbReader
import gui.ClearMods
import gui.LoadMod
import gui.ShipModel
import gui.UnloadMod
import scala.collection.immutable.TreeSet

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

class DataModel extends Publisher with Reactor with Logging {
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
    publish(ReloadFromModel)
  }
  
  def allData = baseGame +: modData

  var customShipDesigns = showErrors(Ship.loadCustomShips(user)).map( ship => (ship.name, ship)).toMap

  val lightningBolt : Image = loadTexture( install / 'Content / 'Textures / 'UI / "lightningBolt.xnb" ).get.img

  def hullsByRace  = allData.map(_.hullsByRace ).reduceLeft(_ ++ _)
  def tokens       = allData.map(_.tokens      ).reduceLeft(_ ++ _)
  def weapons      = allData.map(_.weapons     ).reduceLeft(_ ++ _)
  def modules      = allData.map(_.modules     ).reduceLeft(_ ++ _).filter(_._2.moduleType =/= "Dummy")
  def moduleImages = allData.map(_.moduleImages).reduceLeft(_ ++ _)
  def shipDesigns  = (allData.map(_.shipDesigns ).reduceLeft(_ ++ _) ++ customShipDesigns)
    .filter(_._2.requiredModsList.forall(loadedMods.contains))
  def techsByMod   = allData.map(_.techsByMod  ).reduceLeft(_ ++ _)
  def techsByHull  = allData.map(_.techsByHull ).reduceLeft(_ ++ _)

  def races : Set[String] = TreeSet( hullsByRace.keys.toSeq:_* )

  def hulls(race: String) : Set[Hull] = TreeSet( hullsByRace(race).values.toSeq:_* )( Ordering.by( _.name ) )

  def ships(race: String, hull: String) : Seq[Ship] = shipDesigns.values
      .filter(_.race === race)
      .filter(_.hull === hull)
      .toSeq

  def hullForShip(s: Ship) : Option[Hull] = 
    hullsByRace.get(s.race).flatMap(_.get(s.hull))

  def token(id: Int) : String = tokens.get(id).getOrElse {
    logger.error("Request for unknown localization token: " + id )
    "Invalid Token: " + id
  }

  def weapon(weaponId: String) : Option[Weapon] = weapons.get(weaponId)
  def weaponTypes : Set[String] = weapons.values.map(_.weaponType).toSet

  def module( moduleId: String ) : Option[ShipModule] = modules.get(moduleId)
  def shipModules : Seq[ShipModule] = modules.values.toSeq

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

  def fighterDesigns : Seq[String] =
    shipDesigns.values.filter( _.role === "fighter").map(_.name)
        .toSeq.sorted

  def mods : Seq[Mod] = allMods.values.toSeq.sortBy(_.name)
  def loadedMods : Seq[String] = modData.map(_.name)

  def techsForMods( mods: Iterable[ShipModule]) : Set[Technology] = {
    val techs = techsByMod
    mods.map(_.uid).flatMap(techs.get).toSet
  }

  def techForHull( race: String, hull: String ) : Option[Technology] =
    techsByHull.get( race + "/" + hull )

  def save( ship: ShipModel ) : Option[Ship] = {
    val saved = Ship.saveShip(ship, user)
    saved.foreach{ saved =>
      customShipDesigns += (saved.name -> saved)
    }
    saved
  }

  def saveToFile( ship: ShipModel, file: File ) : Option[Ship] = {
    val saved = Ship.saveShipToFile(ship, file)
    saved.foreach{ saved =>
      customShipDesigns += (saved.name -> saved)
    }
    saved
  }

  reactions += {
    case LoadMod(mod) => modData = modData :+ new ModData(mod.name, install / 'Mods / mod.dir)
    case UnloadMod(mod) => modData = modData.filter(_.name =/= mod.name)
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
      case Right(im) => new ModuleImage(im.asInstanceOf[BufferedImage]).some
    }
  }
}