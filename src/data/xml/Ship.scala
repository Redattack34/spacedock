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
import data.general.FileExtension._
import data.xml.Position.positions
import gui.ModelSlot
import gui.ShipModel
import gui.CombatState
import gui.CombatState._


case class ShipModuleSlot( pos: Position, installed: String, facing: Float,
    slotOptions: Option[String] )
    
case class Ship( name: String, role: String, combatState: Option[CombatState], boardingDefense: Option[Int],
    race: String, hull: String, moduleSlotList: Seq[ShipModuleSlot], requiredModsList: Seq[String]){
}

object Ship extends XmlLoader[Ship]{

  private def slots( e: Elem ) : Seq[ShipModuleSlot] = for {
    moduleSlot <- e \ 'ModuleSlotData
    pos <- positions( moduleSlot )
    module <- moduleSlot \ 'InstalledModuleUID \ text
    facing <- moduleSlot \ 'facing \ text
    slotOptions = moduleSlot \ 'SlotOptions \ text
  } yield ShipModuleSlot(pos, module, 90.0f - facing.toFloat,
      slotOptions.headOption.filter(_ != "NotApplicable"))

  private def mods( e: Elem ) : Seq[String] = for {
    modList <- e \ 'RequiredModList
    mod <- modList \ 'Mod \ text
  } yield mod
  
  def load(f: Option[File], e : Elem) : Seq[Ship] = for {
    name <- e \ 'Name \ text
    role <- e \ 'Role \ text
    combatState = e \ 'CombatState \ text
    boardingDefense = e \ 'MechanicalBoardingDefense \ text
    hull <- e \ 'Hull \ text
    moduleList <- e \ 'ModuleSlotList
    modules = slots(moduleList)
    requiredMods = mods(e)
    split = hull.split('/')
    race = split(0)
    hullId = split(1)
  } yield Ship( name, role, combatState.headOption.flatMap( CombatState.getFromString(_) ),
      boardingDefense.headOption.map(_.toInt), race, hullId, modules, requiredMods)

  def directory(base: File) = base / 'StarterShips
  
  def loadCustomShips( user: File ) : Seq[(File, Option[Ship])] = {
    val savedDesigns = loadFromDirectory(user / "Saved Designs")
    val wip = loadFromDirectory( user / "WIP" )

    savedDesigns ++ wip
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
  
  def getRequiredMods( model: ShipModel ) : Node = {
    val modData = model.ship.requiredModsList.map(getTextNode('Mod, _))
    Elem(None, "RequiredModList", Attributes.empty, Map(), Group(modData:_*))
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
          getRequiredMods(model),
          getTextNode('ModelPath, hull.modelPath),
          getTextNode('DefaultAIState, hull.defaultAIState),
          getModuleSlotList(model)
        ))
  } 
  
  def saveShip( ship: ShipModel, user: File ) : Ship = {
    val xml = shipToXml(ship)
    val dir = if ( ship.hasEmptySlots || !ship.hasCommandModule) "WIP"
              else "Saved Designs"
    val file = user / dir / (ship.ship.name + ".xml")
    val scalaXml = scala.xml.XML.loadString(xml.toString)
    val printer = new PrettyPrinter(200, 2)
    
    val writer = new PrintWriter(new OutputStreamWriter(new FileOutputStream(file), Charsets.UTF_8))
    writer.println("<?xml version=\"1.0\" encoding=\"utf-8\"?>")
    
    val sb = new StringBuilder
    printer.format(scalaXml, sb)
    writer.print(sb)
    writer.close
    
    loadFromFile(file).get
  }
}