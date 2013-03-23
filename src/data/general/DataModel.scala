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

class DataModel(base: File) {

  val hullsByRace = time( "Hulls", loadHulls(base) )
  val tokens = time( "Tokens", loadTokens( new File(base.getAbsolutePath() + "/Content/Localization/English.xml")) )
  val weapons = time( "Weapons", loadWeapons(base) )
  val modules = time( "Modules", loadModules(base).filterNot(_._1 == "Dummy") )
  val moduleImages = time( "Textures", loadModuleTextures)
  var shipDesigns = time( "Ships", loadShips(base))

  val lightningBolt = loadTexture( new File( base.getAbsolutePath() + "/Content/Textures/UI/lightningBolt.xnb" ) ).get

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

  def loadShipFromUrl( url: URL ) : Option[Ship] = {
    val tupleOpt = loadShipsFromUrl(url)
    tupleOpt.foreach( shipDesigns += _)
    tupleOpt.map(_._2)
  }

  private def loadModuleTextures : Map[String, ImageIcon] = {
    val dir = new File( base.getAbsolutePath() + "/Content/Textures/Modules")
    val eithers = for { file <- dir.listFiles().par }
        yield ("Modules/" + file.getName().replace(".xnb", ""), loadTexture(file))

    val successes = eithers.seq.filter(_._2.isDefined).toMap
    successes.mapValues(_.get)
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