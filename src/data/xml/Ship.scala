package data.xml

import java.io.File
import scala.Array.canBuildFrom
import scala.collection.immutable.HashMap
import com.codecommit.antixml.Elem
import com.codecommit.antixml.Selector.symbolToSelector
import com.codecommit.antixml.XML
import com.codecommit.antixml.text
import data.xml.Position._
import java.net.URL


object Ship {

  case class ShipModuleSlot( pos: Position, installed: String, facing: Float,
      slotOptions: Option[String] )

  private def slots( e: Elem ) : Seq[ShipModuleSlot] = for {
    moduleSlot <- e \ 'ModuleSlotData
    pos <- positions( moduleSlot )
    module <- moduleSlot \ 'InstalledModuleUID \ text
    facing <- moduleSlot \ 'facing \ text
    slotOptions = moduleSlot \ 'SlotOptions \ text
  } yield ShipModuleSlot(pos, module, 90.0f - facing.toFloat,
      slotOptions.headOption.filter(_ != "NotApplicable"))

  case class Ship( name: String, role: String, combatState: Option[String], boardingDefense: Option[Int],
    hull: String, moduleSlotList: Seq[ShipModuleSlot])

  private def ships(e : Elem) : Seq[Ship] = for {
    name <- e \ 'Name \ text
    role <- e \ 'Role \ text
    combatState = e \ 'CombatState \ text
    boardingDefense = e \ 'MechanicalBoardingDefense \ text
    hull <- e \ 'Hull \ text
    moduleList <- e \ 'ModuleSlotList
    modules = slots(moduleList)
  } yield Ship( name, role, combatState.headOption, boardingDefense.headOption.map(_.toInt),
      hull, modules)

  def loadShips( install: File, user: File ) : Map[String, Ship] = {
    val shipsDirs = Seq(
          new File(install.getAbsolutePath() + "/Content/StarterShips"),
          new File(user.getAbsolutePath() + "/Saved Designs"),
          new File(user.getAbsolutePath() + "/WIP")
        )
    val allShips = for {
      shipsDir <- shipsDirs
      file <- shipsDir.listFiles().par
      xml = XML.fromInputStream(XmlUtils.read(file))
      ship <- ships(xml)
    } yield (ship.name, ship)
    HashMap(allShips.seq:_*)
  }

  def loadShipsFromUrl( url: URL ) : Option[(String, Ship)] = {
    val xml = XML.fromInputStream(XmlUtils.read(url))
    val allShips = for {
      ship <- ships(xml)
    } yield (ship.name, ship)
    allShips.headOption
  }

  def main(args: Array[String]) {
    val f = new File("C:\\Program Files (x86)\\Steam\\steamapps\\common\\StarDrive\\Content\\StarterShips")
    val allShips = for {
      file <- f.listFiles()
      xml = XML.fromInputStream(XmlUtils.read(file))
      ship = ships(xml)
    } yield (file.getName, ship.headOption)

    allShips.filter(_._2.isEmpty).foreach(f => println("Failed to parse: " + f._1))
    allShips.filter(_._2.isDefined).foreach(t => println( t._1 + ": " + t._2.get.toString ))
  }
}