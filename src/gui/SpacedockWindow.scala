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
import javax.swing.JFileChooser
import scala.io.Source
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.PrintStream

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

  val (install, user) = getDirectories

  val dataModel = new DataModel(install, user)

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

  def isValidInstallDir( f: File ) =
    f != null && f.exists() && f.list.contains("Content")

  def isValidUserDir( f: File ) =
    f != null && f.exists() && f.list.contains("Saved Designs") && f.list.contains("WIP")

  def defaultInstallDir : File = {
    val config = new File("config")
    val dir = if ( !config.exists() || !config.canRead() )
                 "C:\\Program Files (x86)\\Steam\\steamapps\\common\\StarDrive"
              else {
                  val source = Source.fromFile(config)
                  source.getLines.toSeq.head
              }
    new File( dir )
  }

  def defaultUserDir : File = {
    val config = new File("config")
    val dir = if ( !config.exists() || !config.canRead() )
                 System.getProperty("user.home") + "/AppData/Roaming/StarDrive"
              else {
                  val source = Source.fromFile(config)
                  source.getLines.toSeq.apply(1)
              }
    new File( dir )
  }

  private def getDirectories : (File, File) = {
    val chooser = new JFileChooser
    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
    var installDir = defaultInstallDir
    while ( !isValidInstallDir(installDir) ) {
      chooser.setDialogTitle("Select StarDrive Install Directory")
      val accept = chooser.showOpenDialog(null)
      if ( accept == JFileChooser.APPROVE_OPTION ) {
        installDir = chooser.getSelectedFile()
      }
      if ( accept == JFileChooser.CANCEL_OPTION ) {
        System.exit(0)
      }
    }

    println();

    var userDir = defaultUserDir
    while ( !isValidUserDir(userDir) ) {
      chooser.setDialogTitle("Select StarDrive User Directory")
      val accept = chooser.showOpenDialog(null)
      if ( accept == JFileChooser.APPROVE_OPTION ) {
        userDir = chooser.getSelectedFile()
      }
      if ( accept == JFileChooser.CANCEL_OPTION ) {
        System.exit(0)
      }
    }

    val config = new File("config")
    if ( !config.exists() ) config.createNewFile
    val writer = new PrintStream( config )
    writer.println(installDir)
    writer.println(userDir)
    writer.close

    (installDir, userDir)
  }
}