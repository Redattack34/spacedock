package data.general

import scala.swing.Reactor
import java.io.File
import scala.io.Source
import java.io.PrintStream
import javax.swing.JFileChooser

object Config extends Reactor {

  
  
  private[this] var _install : File = null
  def install = _install
  private def install_=(file: File) = {
    _install = file
  }

  private[this] var _user : File = null
  def user = _user
  private def user_=(file: File) = {
    _user = file
  }
  
  private[this] val config = new File("config")
  if ( !config.exists() || !config.canRead() ) {
    val (install, user) = getDirectories
    this.install = install
    this.user = user
  }
  else {
    val source = Source.fromFile(config)
    val lines = source.getLines
    this.install = new File(lines.next)
    this.user = new File(lines.next)
  }
  
  private def isValidInstallDir( f: File ) =
    f != null && f.exists() && f.list.contains("Content")

  private def isValidUserDir( f: File ) =
    f != null && f.exists() && f.list.contains("Saved Designs") && f.list.contains("WIP")

  private def defaultInstallDir : File = {
    new File( "C:\\Program Files (x86)\\Steam\\steamapps\\common\\StarDriver" )
  }

  private def defaultUserDir : File = {
    new File( System.getProperty("user.home") + "/AppData/Roaming/StarDriver" )
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