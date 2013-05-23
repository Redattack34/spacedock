package gui

import java.io.File
import java.net.URL
import javax.swing.JFileChooser
import javax.swing.JMenuItem
import javax.swing.JOptionPane
import javax.swing.filechooser.FileNameExtensionFilter
import scala.swing.ButtonGroup
import scala.swing.CheckMenuItem
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.MenuItem
import scala.swing.RadioButton
import scala.swing.event.ButtonClicked
import scala.swing.event.Event
import scalaz.Scalaz.ToEqualOps
import scalaz.Scalaz.ToOptionIdOps
import scalaz.Scalaz.intInstance
import scalaz.Scalaz.stringInstance
import com.weiglewilczek.slf4s.Logging
import data.general.DataModel
import data.general.ReloadFromModel
import data.xml.Hull
import data.xml.Ship
import data.xnb.XnbReader
import data.general.Config

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
case class ShowGridSet( showGrid: Boolean ) extends Event

class SpacedockMenu( data: DataModel ) extends MenuBar with Logging {

  case class HullMenuItem(val hull: Hull) extends MenuItem("New " + hull.name)
  case class ShipMenuItem(val ship: Ship, val hull: Hull ) extends MenuItem(ship.name)

  case object ZoomMenuItem extends CheckMenuItem("Zoom")
  case object ShowFiringArcsItem extends CheckMenuItem("Show Firing Arcs and Shields")
  case object MirrorItem extends CheckMenuItem("Mirror Changes")
  case object ShowGridItem extends CheckMenuItem("Show Position Grid")
  case object ShowEmptySlotsItem extends MenuItem("Highlight Empty Slots")
  case object FillEmptySlotsItem extends MenuItem("Fill Empty Slots")

  case object LoadShipFromFileItem extends MenuItem("Load From File")
  case object LoadShipFromUrlItem extends MenuItem("Load From URL")
  case object SaveShipItem extends MenuItem("Save")
  case object SaveAsItem extends MenuItem("Save As")
  case object ExitItem extends MenuItem("Exit")
  case object LoadModsItem extends MenuItem("Load Mods")
  case object ExtractTexturesItem extends MenuItem("Extract Textures")

  case class CombatStateButton( cs: CombatState ) extends RadioButton( cs.desc + "  " ) {
    tooltip = data.token(cs.token)
  }

  val fileMenu = new Menu("File")
  fileMenu.contents ++= Seq(LoadShipFromFileItem, LoadShipFromUrlItem, SaveShipItem, SaveAsItem, LoadModsItem, ExitItem )

  listenTo(LoadShipFromFileItem, LoadShipFromUrlItem, SaveShipItem, SaveAsItem , LoadModsItem, ExitItem)

  val attackRuns = CombatStateButton(AttackRuns)
  val artillery = CombatStateButton(Artillery)
  val holdPosition = CombatStateButton(HoldPosition)
  val orbitPort = CombatStateButton(OrbitPort)
  val orbitStarboard = CombatStateButton(OrbitStarboard)
  val evade = CombatStateButton(Evade)

  val group = new ButtonGroup( artillery, attackRuns, holdPosition,
      orbitPort, orbitStarboard, evade );

  val shipsMenu = new Menu("Ships")

  val toolsMenu = new Menu("Tools")
  toolsMenu.contents ++= Seq( ZoomMenuItem, ShowFiringArcsItem, MirrorItem,
      ShowGridItem, ShowEmptySlotsItem, FillEmptySlotsItem, ExtractTexturesItem )
  listenTo( ZoomMenuItem, ShowFiringArcsItem, MirrorItem, ShowGridItem,
    ShowEmptySlotsItem, FillEmptySlotsItem, ExtractTexturesItem )

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
  listenTo(attackRuns, artillery,
      holdPosition, orbitPort, orbitStarboard, evade)


  def shipLoaded(shipOpt: Option[Ship]) : Unit = {
    if ( shipOpt.isEmpty ) {
      JOptionPane.showMessageDialog(this.peer.getParent(),
          "Failed to Load Ship", "Failed to Load Ship", JOptionPane.ERROR_MESSAGE)
    }
    else {
      val ship = shipOpt.get
      val hullOpt = data.hullForShip(ship)
      if ( hullOpt.isEmpty ) { return } //Something very strange has occurred.
      val hull = hullOpt.get

      val menuItem = new ShipMenuItem(ship, hull)
      val menu = hullMenuItems(hull.name)
      val index = menu.peer.getMenuComponents.indexWhere{ _.asInstanceOf[JMenuItem].getText === ship.name }
      if ( index =/= -1 ) {
        menu.peer.remove(index)
      }
      menu.contents.insert(menu.peer.getMenuComponentCount() - 1, menuItem)
      listenTo(menuItem)
      publish(ShipSelected(ship, hull))
    }
  }

  reactions += {
    case ShipSaved(ship) => shipLoaded(ship.some)
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
      if (urlString =/= null) {
        val url = new URL( urlString )
        shipLoaded(data.loadShipFromUrl(url))
      }
    }
    case ButtonClicked(LoadShipFromFileItem) => {
      val accepted = chooser.showOpenDialog(this.peer.getParent())
      if (accepted === JFileChooser.APPROVE_OPTION) {
        val file = chooser.getSelectedFile()
        val shipOpt = data.loadShipFromFile(file)
        shipLoaded(shipOpt)
      }
    }
    case ButtonClicked(SaveShipItem) => publish(SaveShip)
    case ButtonClicked(SaveAsItem) => {
      val accepted = chooser.showSaveDialog(this.peer.getParent())
      if (accepted === JFileChooser.APPROVE_OPTION) {
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
    case ButtonClicked(ShowGridItem) => publish( ShowGridSet( ShowGridItem.selected ) )
    case ButtonClicked(ShowEmptySlotsItem) => publish( ShowEmptySlots )
    case ButtonClicked(FillEmptySlotsItem) => publish( FillEmptySlots )
    case ButtonClicked(LoadModsItem) => publish( OpenModWindow )
    case ButtonClicked(CombatStateButton(cs)) => publish( CombatStateSet(cs))
    case ButtonClicked(ExtractTexturesItem) => {
      val tempChooser = new JFileChooser(Config.install)
      tempChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES)
      tempChooser.setDialogTitle("Select Texture File(s) or Folder(s)")
      tempChooser.setMultiSelectionEnabled(true)
      val accepted = tempChooser.showDialog(this.peer.getParent(), "Extract")
      if (accepted === JFileChooser.APPROVE_OPTION) {
        val files = tempChooser.getSelectedFiles()
        val errors = XnbReader.extractImages(files:_*)
        errors.foreach{ case (file, error) =>
          logger.error( "Failed to read " + file + ": ", error)
        }
        if ( !errors.isEmpty ) {
          val errorString = "Failed to extract textures (See log for more details):\n" +
            errors.map{ case(file, error) => file.getName() + ": " + error.getMessage }.mkString("\n")
          JOptionPane.showMessageDialog(this.peer.getParent(), errorString,
              "Spacedock: TextureExtractionFailed", JOptionPane.ERROR_MESSAGE);
        }
        else {
          JOptionPane.showMessageDialog(this.peer.getParent(), "Extraction Complete",
              "Spacedock: Texture Extraction Successful", JOptionPane.INFORMATION_MESSAGE);
        }
      }
    }
    case ReloadFromModel => {
      shipsMenu.contents.clear
      loadMenus
    }
  }
}