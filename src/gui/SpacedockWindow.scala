package gui

import javax.swing.UIManager
import scala.swing.Component
import scala.swing.MainFrame
import scala.swing.MenuBar
import scala.swing.Orientation
import scala.swing.ScrollPane
import scala.swing.SimpleSwingApplication
import scala.swing.SplitPane
import scala.swing.Window
import scalaz.Scalaz._
import data.general.DataModel
import util.ExceptionHandler
import scala.swing.Panel
import scala.swing.GridBagPanel

object Spacedock extends SimpleSwingApplication {

  try {
    new ExceptionHandler().installExceptionHandler();
    UIManager.setLookAndFeel( UIManager.getSystemLookAndFeelClassName() );
    for (info <- UIManager.getInstalledLookAndFeels( ))
    {
      if ( "Nimbus" === info.getName( ) ) {
        UIManager.setLookAndFeel( info.getClassName( ) );
      }
    }
  }
  catch {
    case ex : Exception => ex.printStackTrace()
  }

  val dataModel = new DataModel

  val modules: Component = new ModuleList(dataModel)
  val shipEditor: Component = new ShipEditor(dataModel)
  val editorScroll: ScrollPane = new ScrollPane(shipEditor){ verticalScrollBar.unitIncrement = 16 }
  val shipStats: Component = new ShipStats(dataModel)
  val moduleStats: Component = new ModuleStats(dataModel)
  val simStats: Component = new SimulatorPanel(dataModel)
  val sdMenuBar: MenuBar = new SpacedockMenu(dataModel)
  val modWindow: Window = new ModWindow(dataModel)

  moduleStats.listenTo(modules)

  shipEditor.listenTo(sdMenuBar)
  shipEditor.listenTo(modules)
  shipEditor.listenTo(dataModel)

  modules.listenTo(shipEditor)
  modules.listenTo(dataModel)

  sdMenuBar.listenTo(shipEditor)
  sdMenuBar.listenTo(dataModel)

  shipStats.listenTo(shipEditor)

  modWindow.listenTo(sdMenuBar)

  simStats.listenTo(shipEditor)

  this.listenTo(shipEditor)
  this.listenTo(sdMenuBar)

  dataModel.listenTo(modWindow)

  val toolPaneLeft = new SplitPane {
    topComponent = simStats
    bottomComponent = shipStats
    oneTouchExpandable = true
    resizeWeight = 0.5
  }

  val toolPaneRight = new SplitPane {
    oneTouchExpandable = true
    resizeWeight = 0.25
    topComponent = modules
    bottomComponent = moduleStats
  }

  val rightSplit = new SplitPane {
    orientation = Orientation.Vertical
    resizeWeight = 0.8
    oneTouchExpandable = true
    leftComponent = editorScroll
    rightComponent = toolPaneRight
  }

  val leftSplit = new SplitPane {
    orientation = Orientation.Vertical
    resizeWeight = 0.2
    oneTouchExpandable = true
    leftComponent = toolPaneLeft
    rightComponent = rightSplit
  }

  val top = new MainFrame {
    title = "Spacedock"
    menuBar = sdMenuBar
    contents = leftSplit
    maximize
  }

  reactions += {
    case ShipModelChanged(newModel) => top.title = "Spacedock: " + newModel.ship.name
    case ZoomSet(zoom) => editorScroll.verticalScrollBar.unitIncrement = if (zoom) 32 else 16
  }
}