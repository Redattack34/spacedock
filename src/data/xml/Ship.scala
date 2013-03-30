package data.xml

import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.net.URL

import scala.Array.canBuildFrom
import scala.xml.PrettyPrinter

import com.codecommit.antixml.Attributes
import com.codecommit.antixml.Elem
import com.codecommit.antixml.Group
import com.codecommit.antixml.Node
import com.codecommit.antixml.Selector.symbolToSelector
import com.codecommit.antixml.Text
import com.codecommit.antixml.XML
import com.codecommit.antixml.stringTupleToQNameTuple
import com.codecommit.antixml.text
import com.google.common.base.Charsets

import data.general.DataModel
import data.xml.Position.positions
import gui.ModelSlot
import gui.ShipModel


case class ShipModuleSlot( pos: Position, installed: String, facing: Float,
		slotOptions: Option[String] )
		
case class Ship( name: String, role: String, combatState: Option[String], boardingDefense: Option[Int],
		race: String, hull: String, moduleSlotList: Seq[ShipModuleSlot])

object Ship {

  private def slots( e: Elem ) : Seq[ShipModuleSlot] = for {
    moduleSlot <- e \ 'ModuleSlotData
    pos <- positions( moduleSlot )
    module <- moduleSlot \ 'InstalledModuleUID \ text
    facing <- moduleSlot \ 'facing \ text
    slotOptions = moduleSlot \ 'SlotOptions \ text
  } yield ShipModuleSlot(pos, module, 90.0f - facing.toFloat,
      slotOptions.headOption.filter(_ != "NotApplicable"))

  private def ships(e : Elem) : Seq[Ship] = for {
    name <- e \ 'Name \ text
    role <- e \ 'Role \ text
    combatState = e \ 'CombatState \ text
    boardingDefense = e \ 'MechanicalBoardingDefense \ text
    hull <- e \ 'Hull \ text
    moduleList <- e \ 'ModuleSlotList
    modules = slots(moduleList)
    split = hull.split('/')
    race = split(0)
    hullId = split(1)
  } yield Ship( name, role, combatState.headOption, boardingDefense.headOption.map(_.toInt),
      race, hullId, modules)

  def loadShips( install: File ) : Seq[(File, Option[Ship])] = {
    val shipsDirs = Seq(
          new File(install.getAbsolutePath() + "/StarterShips")
        )
    val allShips = for {
      shipsDir <- shipsDirs
      file <- shipsDir.listFiles().par
      xml = XML.fromInputStream(XmlUtils.read(file))
      ship = ships(xml)
    } yield (file, ship.headOption)
    allShips.seq
  }
  
  def loadCustomShips( user: File ) : Seq[(File, Option[Ship])] = {
    val shipsDirs = Seq(
          new File(user.getAbsolutePath() + "/Saved Designs"),
          new File(user.getAbsolutePath() + "/WIP")
        )
    val allShips = for {
      shipsDir <- shipsDirs
      file <- shipsDir.listFiles().par
      xml = XML.fromInputStream(XmlUtils.read(file))
      ship = ships(xml)
    } yield (file, ship.headOption)
    allShips.seq
  }
 
  def loadShipsFromFile( f: File ) : Option[(String, Ship)] = {
    val xml = XML.fromInputStream(XmlUtils.read(f))
    val allShips = for {
      ship <- ships(xml)
    } yield (ship.name, ship)
    allShips.headOption
  }
  
  def loadShipsFromUrl( url: URL ) : Option[(String, Ship)] = {
    val xml = XML.fromInputStream(XmlUtils.read(url))
    val allShips = for {
      ship <- ships(xml)
    } yield (ship.name, ship)
    allShips.headOption
  }

  private def getTextNode( name: Symbol, text: Any ) = {
    Elem(None, name.name, Attributes.empty, Map(), Group(Text(text.toString)))
  }
  
  def getPosition( pos: Position ) : Node = {
    Elem(None, "Position", Attributes.empty, Map(), Group( getTextNode('X, pos.x), getTextNode('Y, pos.y)) )
  }
  
  def getThrusterList( hull: Hull ) : Node = {
    val zones = hull.thrusterList.map{ zone =>
      Elem(None, "ThrusterZone", Attributes.empty, Map(), Group( getPosition(zone.pos), getTextNode('scale, zone.scale)))
    }
    Elem(None, "ThrusterList", Attributes.empty, Map(), Group(zones:_*))
  }
  
  def getModuleSlotData( slot: ModelSlot ) : Node = {
    var elem = Elem(None, "ModuleSlotData", Attributes.empty, Map(), Group(
        getPosition(slot.hullSlot.pos),
        getTextNode('InstalledModuleUID, slot.module.uid),
        getTextNode('HangarshipGuid, "00000000-0000-0000-0000-000000000000"),
        getTextNode('Health, 0),
        getTextNode('Shield_Power, 0),
        getTextNode('facing, slot.facing - 90.0),
        getTextNode('Restrictions, slot.hullSlot.restrictions)
    ))
    
    if ( slot.slotOption.isDefined ) {
      elem = Elem( elem.prefix, elem.name, elem.attrs, elem.scope, 
          elem.children :+ getTextNode('SlotOptions, slot.slotOption.get) )
    }
    elem
  }
  
  def getModuleSlotList( ship: ShipModel ) : Node = {
    val slotData = ship.slots.values.toSeq.map(getModuleSlotData)
    Elem(None, "ModuleSlotList", Attributes.empty, Map(), Group(slotData:_*))
  }
  
  private def shipToXml( model: ShipModel ) : Node = {
    val ship = model.ship
    val hull = model.hull
    val combatState = model.combatState
    Elem(None, "ShipData", Attributes("xmlns:xsi" -> "http://www.w3.org/2001/XMLSchema-instance", "xmlns:xsd" -> "http://www.w3.org/2001/XMLSchema" ), Map(), 
        Group(
          getTextNode('Animated, false),
          getTextNode('ShipStyle, ship.race),
          getTextNode('experience, 0),
          getTextNode('Level, 0),
          getTextNode('Name, ship.name),
          getTextNode('HasFixedCost, false),
          getTextNode('FixedCost, 0),
          getTextNode('IsShipyard, false),
          getTextNode('IsOrbitalDefense, false),
          getTextNode('IconPath, hull.iconPath),
          getTextNode('CombatState, combatState),
          getTextNode('MechanicalBoardingDefense, 0),
          getTextNode('Hull, ship.race + "/" + ship.hull),
          getTextNode('Role, hull.role),
          getThrusterList(hull),
          getTextNode('ModelPath, hull.modelPath),
          getTextNode('DefaultAIState, hull.defaultAIState),
          getModuleSlotList(model)
        ))
  } 
  
  def saveShip( ship: ShipModel, user: File ) : (String, Ship) = {
    val xml = shipToXml(ship)
    val dir = if ( ship.hasEmptySlots || !ship.hasCommandModule) "/WIP/"
              else "/Saved Designs/"
    val file = new File( user.getAbsolutePath() + dir + ship.ship.name + ".xml")
    val scalaXml = scala.xml.XML.loadString(xml.toString)
    val printer = new PrettyPrinter(200, 2)
    
    val writer = new PrintWriter(new OutputStreamWriter(new FileOutputStream(file), Charsets.UTF_8))
    writer.println("<?xml version=\"1.0\" encoding=\"utf-8\"?>")
    
    val sb = new StringBuilder
    printer.format(scalaXml, sb)
    writer.print(sb)
    writer.close
    
    loadShipsFromFile(file).get
  }

  def testSerialization = {
    val hullFile = new File("C:\\Program Files (x86)\\Steam\\steamapps\\common\\StarDrive\\Content\\Hulls\\Terran\\Hunter.xml")
    val shipFile = new File("C:\\Users\\Redattack34\\AppData\\Roaming\\StarDrive\\Saved Designs\\Broadsword Mk II.xml")
    
    val ship = ships( XML.fromInputStream(XmlUtils.read(shipFile))).head
    val hull = Hull.hulls( "Terran", "Broadsword Mk II", XML.fromInputStream(XmlUtils.read(hullFile))).head
        
    val dataModel = new DataModel
    val xml = shipToXml(ShipModel(dataModel, hull, ship))
    val test = scala.xml.XML.loadString(xml.toString)
    val printer = new PrettyPrinter(200, 2)
    val sb = new StringBuilder()
    printer.format(test, sb)
    println(sb);
  }
  
  def testReading = {
        val f = new File("C:\\Users\\Redattack34\\AppData\\Roaming\\StarDrive\\Saved Designs")
    val allShips = for {
      file <- f.listFiles()
      xml = XML.fromInputStream(XmlUtils.read(file))
      ship = ships(xml)
    } yield (file.getName, ship.headOption)

    allShips.filter(_._2.isEmpty).foreach(f => println("Failed to parse: " + f._1))
    allShips.filter(_._2.isDefined).foreach(t => println( t._1 + ": " + t._2.get.toString ))
  } 
  
  def main(args: Array[String]) {
    //testSerialization
    testReading
  }
}