package gui

import java.io.File

import scala.swing.CheckMenuItem
import scala.swing.Component
import scala.swing.MainFrame
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.MenuItem
import scala.swing.Orientation
import scala.swing.Panel
import scala.swing.ScrollPane
import scala.swing.SimpleSwingApplication
import scala.swing.SplitPane

import data.general.DataModel
import data.xml.Hull.Hull
import javax.swing.UIManager

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

  val base = new File("C:\\Program Files (x86)\\Steam\\steamapps\\common\\StarDrive")

  val dataModel = new DataModel(base)

  val modules: Component = new ModuleList(dataModel)
  val shipEditor: Component = new ShipEditor(dataModel)
  val shipStats: Component = new ShipStats()
  val moduleStats: Component = new ModuleStats(dataModel)
  val sdMenuBar: MenuBar = new SpacedockMenu(dataModel)

  moduleStats.listenTo(modules)

  shipEditor.listenTo(sdMenuBar)
  shipEditor.listenTo(modules)

  modules.listenTo(shipEditor)

  shipStats.listenTo(shipEditor)

  val toolPaneBottom = new SplitPane {
    topComponent = moduleStats
    bottomComponent = shipStats
    oneTouchExpandable = true
    resizeWeight = 0.5
  }

  val toolPaneTop = new SplitPane {
    oneTouchExpandable = true
    resizeWeight = 0.33
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

  def top = new MainFrame {
    title = "Spacedock"
    menuBar = sdMenuBar
    contents = mainSplit
  }
}