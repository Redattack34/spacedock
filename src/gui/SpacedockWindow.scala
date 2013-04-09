package gui

import scala.swing.Component
import scala.swing.MainFrame
import scala.swing.MenuBar
import scala.swing.Orientation
import scala.swing.ScrollPane
import scala.swing.SimpleSwingApplication
import scala.swing.SplitPane
import data.general.DataModel
import javax.swing.UIManager
import scala.swing.Window
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.core.util.StatusPrinter
import org.slf4j.LoggerFactory

object Spacedock extends SimpleSwingApplication {

  try {
    UIManager.setLookAndFeel( UIManager.getSystemLookAndFeelClassName() );
    for (info <- UIManager.getInstalledLookAndFeels( ))
    {
      if ( "Nimbus" == info.getName( ) ) {
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
  val shipStats: Component = new ShipStats(dataModel)
  val moduleStats: Component = new ModuleStats(dataModel)
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

  this.listenTo(shipEditor)

  dataModel.listenTo(modWindow)

  val toolPaneBottom = new SplitPane {
    topComponent = moduleStats
    bottomComponent = shipStats
    oneTouchExpandable = true
    resizeWeight = 0.5
  }

  val toolPaneTop = new SplitPane {
    oneTouchExpandable = true
    resizeWeight = 0.0
    topComponent = modules
    bottomComponent = toolPaneBottom
  }

  val mainSplit = new SplitPane {
    orientation = Orientation.Vertical
    resizeWeight = 0.8
    oneTouchExpandable = true
    leftComponent = new ScrollPane( shipEditor )
    rightComponent = toolPaneTop
  }

  val top = new MainFrame {
    title = "Spacedock"
    menuBar = sdMenuBar
    contents = mainSplit
    maximize
  }

  reactions += {
    case ShipModelChanged(newModel) => top.title = "Spacedock: " + newModel.ship.name
  }
}