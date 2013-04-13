package gui

import data.general.DataModel
import scala.swing.RadioButton
import data.xml.Ship
import data.xml.Hull
import javax.swing.JOptionPane
import scala.swing.CheckMenuItem
import scala.swing.event.Event
import scala.swing.MenuItem
import scala.swing.Menu
import scala.swing.ButtonGroup
import scala.swing.MenuBar
import scala.swing.event.ButtonClicked
import java.net.URL
import javax.swing.JFileChooser
import javax.swing.filechooser.FileNameExtensionFilter
import data.general.ReloadFromModel
import java.io.File


case class HullSelected( hull: Hull ) extends Event
case class ShipSelected( ship: Ship, hull: Hull ) extends Event
case class ZoomSet( zoom: Boolean ) extends Event
case class FiringArcsSet( showFiringArcs: Boolean ) extends Event
case class MirroringSet( mirror: Boolean ) extends Event
case object ShowEmptySlots extends Event
case object FillEmptySlots extends Event
case class CombatStateSet( state: CombatState ) extends Event
case object SaveShip extends Event
case class SaveAs(file: File) extends Event
case object OpenModWindow extends Event

class SpacedockMenu( data: DataModel ) extends MenuBar {

  case class HullMenuItem(val hull: Hull) extends MenuItem("New " + hull.name)
  case class ShipMenuItem(val ship: Ship, val hull: Hull ) extends MenuItem(ship.name)

  case object ZoomMenuItem extends CheckMenuItem("Zoom")
  case object ShowFiringArcsItem extends CheckMenuItem("Show Firing Arcs and Shields")
  case object MirrorItem extends CheckMenuItem("Mirror Changes")
  case object ShowEmptySlotsItem extends MenuItem("Highlight Empty Slots")
  case object FillEmptySlotsItem extends MenuItem("Fill Empty Slots")

  case object LoadShipFromFileItem extends MenuItem("Load From File")
  case object LoadShipFromUrlItem extends MenuItem("Load From URL")
  case object SaveShipItem extends MenuItem("Save")
  case object SaveAsItem extends MenuItem("Save As")
  case object ExitItem extends MenuItem("Exit")
  case object LoadModsItem extends MenuItem("Load Mods")

  val fileMenu = new Menu("File")
  fileMenu.contents ++= Seq(LoadShipFromFileItem, LoadShipFromUrlItem, SaveShipItem, SaveAsItem, LoadModsItem, ExitItem )

  listenTo(LoadShipFromFileItem, LoadShipFromUrlItem, SaveShipItem, SaveAsItem , LoadModsItem, ExitItem)

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

  val toolsMenu = new Menu("Tools")
  toolsMenu.contents ++= Seq( ZoomMenuItem, ShowFiringArcsItem, MirrorItem,
      ShowEmptySlotsItem, FillEmptySlotsItem )

  var hullMenuItems = Map[String, Menu]()

  val chooser = new JFileChooser
  chooser.setFileFilter(new FileNameExtensionFilter("Ship XML Files", "xml"))
  chooser.setFileSelectionMode(JFileChooser.FILES_ONLY)

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
        val hullMenu = new Menu(hull.name + " (" + hull.role + ")")
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

  contents ++= Seq( fileMenu, shipsMenu, toolsMenu, attackRuns, artillery,
      holdPosition, orbitPort, orbitStarboard, evade)
  listenTo( ZoomMenuItem, ShowFiringArcsItem, MirrorItem, ShowEmptySlotsItem,
      FillEmptySlotsItem, attackRuns, artillery, holdPosition, orbitPort, orbitStarboard, evade)

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
      publish( ShipSelected(ship, hull) )
      ship.combatState match {
        case Some(Artillery) => artillery.selected = true
        case Some(HoldPosition) => holdPosition.selected = true
        case Some(Evade) => evade.selected = true
        case Some(OrbitPort) => orbitPort.selected = true
        case Some(OrbitStarboard) => orbitStarboard.selected = true
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
      val accepted = chooser.showOpenDialog(this.peer.getParent())
      if (accepted == JFileChooser.APPROVE_OPTION) {
        val file = chooser.getSelectedFile()
        val shipOpt = data.loadShipFromFile(file)
        shipLoaded(shipOpt)
      }
    }
    case ButtonClicked(SaveShipItem) => publish(SaveShip)
    case ButtonClicked(SaveAsItem) => {
      val accepted = chooser.showSaveDialog(this.peer.getParent())
      if (accepted == JFileChooser.APPROVE_OPTION) {
        val file = chooser.getSelectedFile()
        val withExtension = if ( !file.getName().endsWith(".xml") ) new File( file.getAbsolutePath() + ".xml" )
                            else file
        publish( SaveAs(withExtension) )
      }
    }
    case ButtonClicked(ExitItem) => System.exit(0)
    case ButtonClicked(ZoomMenuItem) => publish( ZoomSet( ZoomMenuItem.selected ) )
    case ButtonClicked(ShowFiringArcsItem) => publish( FiringArcsSet( ShowFiringArcsItem.selected ) )
    case ButtonClicked(MirrorItem) => publish( MirroringSet( MirrorItem.selected ) )
    case ButtonClicked(ShowEmptySlotsItem) => publish( ShowEmptySlots )
    case ButtonClicked(FillEmptySlotsItem) => publish( FillEmptySlots )
    case ButtonClicked(LoadModsItem) => publish( OpenModWindow )
    case ButtonClicked(rb) if rb == attackRuns => publish( CombatStateSet(AttackRuns))
    case ButtonClicked(rb) if rb == artillery => publish( CombatStateSet(Artillery))
    case ButtonClicked(rb) if rb == holdPosition => publish( CombatStateSet(HoldPosition))
    case ButtonClicked(rb) if rb == orbitPort => publish( CombatStateSet(OrbitPort))
    case ButtonClicked(rb) if rb == orbitStarboard => publish( CombatStateSet(OrbitStarboard))
    case ButtonClicked(rb) if rb == evade => publish( CombatStateSet(Evade))
    case ReloadFromModel => {
      shipsMenu.contents.clear
      loadMenus
    }
  }
}