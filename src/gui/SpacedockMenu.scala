package gui

import scala.swing.CheckMenuItem
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.MenuItem
import scala.swing.event.ButtonClicked
import scala.swing.event.Event
import data.general.DataModel
import data.xml.Hull.Hull
import data.xml.Ship.Ship
import scala.swing.CheckBox
import scala.swing.RadioButton
import scala.swing.ButtonGroup
import javax.swing.JOptionPane
import java.net.URI
import java.net.URL
import javax.swing.JFileChooser
import javax.swing.filechooser.FileNameExtensionFilter

case class HullSelected( hull: Hull ) extends Event
case class ShipSelected( ship: Ship, hull: Hull ) extends Event
case class ZoomSet( zoom: Boolean ) extends Event
case class FiringArcsSet( showFiringArcs: Boolean ) extends Event
case class CombatStateSet( str: String ) extends Event
case object SaveShip extends Event

class SpacedockMenu( data: DataModel ) extends MenuBar {

  case class HullMenuItem(val hull: Hull, str: String) extends MenuItem(str)
  case class ShipMenuItem(val ship: Ship, val hull: Hull ) extends MenuItem(ship.name)
  case object ZoomMenuItem extends CheckBox("Zoom  ")
  case object ShowFiringArcsItem extends CheckBox("Show Firing Arcs  ")
  
  case object LoadShipFromFileItem extends MenuItem("Load From File")
  case object LoadShipFromUrlItem extends MenuItem("Load From URL")
  case object SaveShipItem extends MenuItem("Save")
  case object ExitItem extends MenuItem("Exit")
  
  val fileMenu = new Menu("File")
  fileMenu.contents ++= Seq( LoadShipFromFileItem, LoadShipFromUrlItem, SaveShipItem, ExitItem )
  this.contents += fileMenu
  
  val attackRuns = new RadioButton("Attack Runs  ")
  attackRuns.tooltip = data.tokens(200)

  val artillery = new RadioButton("Artillery  ")
  artillery.tooltip = data.tokens(201)

  val holdPosition = new RadioButton("Hold Position  ")
  holdPosition.tooltip = data.tokens(263)

  val orbitPort = new RadioButton("Orbit Port  ")
  orbitPort.tooltip = data.tokens(202)

  val orbitStarboard = new RadioButton("Orbit Starboard  ")
  orbitStarboard.tooltip = data.tokens(203)

  val evade = new RadioButton("Evade")
  evade.tooltip = data.tokens(205)

  val group = new ButtonGroup( artillery, attackRuns, holdPosition,
      orbitPort, orbitStarboard, evade );

  val loadHull = new Menu("Load Hull")
  contents += loadHull

  var shipMap = Map[String, Menu]()

  def addRaceHulls( race: String, raceMenu: Menu ) = {
    val hulls = data.hullsByRace(race).toSeq.sortBy(_._1)
    hulls.foreach { tuple =>
      val hull = new HullMenuItem( tuple._2, tuple._1)
      listenTo(hull)
      raceMenu.contents += hull
    }
  }

  def addHulls = {
    val races = data.hullsByRace.keys.toSeq.sortBy(identity)
    races.foreach { race =>
      val raceMenu = new Menu(race)
      loadHull.contents += raceMenu
      addRaceHulls( race, raceMenu )
    }
  }

  addHulls

  val loadShip = new Menu("Load Ship")
  contents += loadShip

  def addHullShips( race: String, hull: Hull, hullMenu : Menu ) = {
    val hullId = race + "/" + hull.name
    val ships = data.shipDesigns.values.filter(_.hull == hullId).toSeq.sortBy(_.name)
    ships.foreach { ship =>
      val menuItem = ShipMenuItem( ship, hull )
      hullMenu.contents += menuItem
      listenTo(menuItem)
    }
  }

  def addRaceShips( race: String, raceMenu: Menu ) = {
    val hulls = data.hullsByRace(race).toSeq.sortBy(_._1)
    hulls.foreach { tuple =>
      val menu = new Menu( tuple._1 )
      shipMap += (tuple._1 -> menu)
      addHullShips( race, tuple._2, menu )
      raceMenu.contents += menu
    }
  }

  def addShips = {
    val races = data.hullsByRace.keys.toSeq.sortBy(identity)
    races.foreach { race =>
      val raceMenu = new Menu(race)
      loadShip.contents += raceMenu
      addRaceShips( race, raceMenu )
    }
  }

  addShips

  listenTo(LoadShipFromFileItem, LoadShipFromUrlItem, SaveShipItem, ExitItem)

  contents ++= Seq( ZoomMenuItem, ShowFiringArcsItem, attackRuns,
      artillery, holdPosition, orbitPort, orbitStarboard, evade)
  listenTo( ZoomMenuItem, ShowFiringArcsItem, attackRuns, artillery,
      holdPosition, orbitPort, orbitStarboard, evade)

  def shipLoaded(shipOpt: Option[Ship]) = {
    if ( shipOpt.isEmpty ) {
      JOptionPane.showMessageDialog(this.peer.getParent(),
          "Failed to Load Ship", "Failed to Load Ship", JOptionPane.ERROR_MESSAGE)
    }
    else {
      val ship = shipOpt.get
      val split = ship.hull.split("/")
      val race = split(0)
      val hullName = split(1)
      val hull = data.hullsByRace(race)(hullName)
      val menuItem = new ShipMenuItem(ship, hull)
      shipMap(hullName).contents += menuItem
      listenTo(menuItem)
      publish(ShipSelected(ship, hull))
    }
  }
      
  reactions += {
    case ShipSaved(ship) => shipLoaded(Some(ship))
    case ButtonClicked(HullMenuItem(hull, _)) => publish( HullSelected(hull) )
    case ButtonClicked(ShipMenuItem(ship, hull)) => {
      publish( ShipSelected(ship, hull))
      ship.combatState match {
        case Some("Artillery") => artillery.selected = true
        case Some("HoldPosition") => holdPosition.selected = true
        case Some("Evade") => evade.selected = true
        case Some("OrbitLeft") => orbitPort.selected = true
        case Some("OrbitRight") => orbitStarboard.selected = true
        case _ => attackRuns.selected = true
      }
    }
    case ButtonClicked(LoadShipFromUrlItem) => {
      val urlString = JOptionPane.showInputDialog(this.peer.getParent(),
          "Enter URL:", "Load Ship From URL", JOptionPane.QUESTION_MESSAGE)
      if (urlString != null) {
        val url = new URL( urlString )
        shipLoaded(data.loadShipFromUrl(url))
      }
    }
    case ButtonClicked(LoadShipFromFileItem) => {
      val chooser = new JFileChooser
      chooser.setFileFilter(new FileNameExtensionFilter("Ship XML Files", "xml"))
      chooser.setFileSelectionMode(JFileChooser.FILES_ONLY)
      val accepted = chooser.showOpenDialog(this.peer.getParent())
      if (accepted == JFileChooser.APPROVE_OPTION) {
        val file = chooser.getSelectedFile()
        val shipOpt = data.loadShipFromFile(file)
        shipLoaded(shipOpt)
      }
    }
    case ButtonClicked(SaveShipItem) => publish(SaveShip)
    case ButtonClicked(ExitItem) => System.exit(0)
    case ButtonClicked(ZoomMenuItem) => publish( ZoomSet( ZoomMenuItem.selected ) )
    case ButtonClicked(ShowFiringArcsItem) => publish( FiringArcsSet( ShowFiringArcsItem.selected ) )
    case ButtonClicked(rb) if rb == attackRuns => publish( CombatStateSet("AttackRuns"))
    case ButtonClicked(rb) if rb == artillery => publish( CombatStateSet("Artillery"))
    case ButtonClicked(rb) if rb == holdPosition => publish( CombatStateSet("HoldPosition"))
    case ButtonClicked(rb) if rb == orbitPort => publish( CombatStateSet("OrbitLeft"))
    case ButtonClicked(rb) if rb == orbitStarboard => publish( CombatStateSet("OrbitRight"))
    case ButtonClicked(rb) if rb == evade => publish( CombatStateSet("Evade"))
  }
}