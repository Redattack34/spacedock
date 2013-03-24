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

class DataModel(install: File, user: File) {

  val hullsByRace : Map[String, Map[String, Hull]] = time( "Hulls", loadHulls(install) )
  val tokens : Map[Int, String] = time( "Tokens", loadTokens( new File(install.getAbsolutePath() + "/Content/Localization/English.xml")) )
  val weapons : Map[String, Weapon] = time( "Weapons", loadWeapons(install) )
  val modules : Map[String, ShipModule] = time( "Modules", loadModules(install).filterNot(_._1 == "Dummy") )
  val moduleImages : Map[String, ImageIcon] = time( "Textures", loadModuleTextures)
  var shipDesigns : Map[String, Ship] = time( "Ships", loadShips(install, user))

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

  private def time[T]( str: String, f: => T ) : T = {
    val watch = new Stopwatch
    watch.start
    val ret = f
    watch.stop
    println(str + " Elapsed Time: " + watch.elapsed(TimeUnit.MILLISECONDS))
    ret
  }
}