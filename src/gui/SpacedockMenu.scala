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

  case class HullMenuItem(val hull: Hull) extends MenuItem("New " + hull.name)
  case class ShipMenuItem(val ship: Ship, val hull: Hull ) extends MenuItem(ship.name)
  case object ZoomMenuItem extends CheckBox("Zoom  ")
  case object ShowFiringArcsItem extends CheckBox("Show Firing Arcs and Shields  ")
  
  case object LoadShipFromFileItem extends MenuItem("Load From File")
  case object LoadShipFromUrlItem extends MenuItem("Load From URL")
  case object SaveShipItem extends MenuItem("Save")
  case object ExitItem extends MenuItem("Exit")
  
  val fileMenu = new Menu("File")
  fileMenu.contents ++= Seq( LoadShipFromFileItem, LoadShipFromUrlItem, SaveShipItem, ExitItem )
  this.contents += fileMenu
  
  val attackRuns = new RadioButton("Attack Runs  ")
  attackRuns.tooltip = data.token(200)

  val artillery = new RadioButton("Artillery  ")
  artillery.tooltip = data.token(201)

  val holdPosition = new RadioButton("Hold Position  ")
  holdPosition.tooltip = data.token(263)

  val orbitPort = new RadioButton("Orbit Port  ")
  orbitPort.tooltip = data.token(202)

  val orbitStarboard = new RadioButton("Orbit Starboard  ")
  orbitStarboard.tooltip = data.token(203)

  val evade = new RadioButton("Evade")
  evade.tooltip = data.token(205)

  val group = new ButtonGroup( artillery, attackRuns, holdPosition,
      orbitPort, orbitStarboard, evade );

  
  val shipsMenu = new Menu("Ships")
  contents += shipsMenu
  
  var hullMenuItems = Map[String, Menu]()

  loadMenus
  def loadMenus = {
    for {
      race <- data.races
    } {
      val raceMenu = new Menu(race + "...")
      shipsMenu.contents += raceMenu
    
      for {
        hull <- data.hulls(race)
      } {
        val hullMenu = new Menu(hull.name + "...")
        raceMenu.contents += hullMenu
        hullMenuItems += (hull.name -> hullMenu)
      
        for {
          ship <- data.ships(race, hull.hullId)
        } {
          val shipMenuItem = new ShipMenuItem(ship, hull)
          hullMenu.contents += shipMenuItem
          listenTo(shipMenuItem)
        }
      
        val hullItem = new HullMenuItem(hull)
        listenTo(hullItem)
        hullMenu.contents += hullItem 
      }
    }
  }
  
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
      val hull = data.hullForShip(ship)
      val menuItem = new ShipMenuItem(ship, hull)
      val menu = hullMenuItems(hull.name)
      menu.contents.insert(menu.peer.getMenuComponentCount() - 1, menuItem)
      listenTo(menuItem)
      publish(ShipSelected(ship, hull))
    }
  }
      
  reactions += {
    case ShipSaved(ship) => shipLoaded(Some(ship))
    case ButtonClicked(HullMenuItem(hull)) => publish( HullSelected(hull) )
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