package data.general

import java.io.File
import java.io.PrintStream

import scala.io.Source

import javax.swing.JFileChooser

import Iterator._

object Config {
  
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
  
  private[this] var _mods : Seq[String] = Vector()
  def mods = _mods
  private def mods_=(newMods: Seq[String]) = {
    _mods = newMods
  }
  
  val chooser = new JFileChooser
  chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
  
  def addMod( name: String ) = {
    mods = mods ++ Seq(name)
    write
  }
  
  def removeMod( name: String ) = {
    mods = mods.filterNot(_ == name)
    write
  }
  
  def clearMods = {
    mods = Vector()
    write
  }
  
  private[this] val config = new File("config")
  if ( !config.exists() || !config.canRead() ) {
    this.install = installDirs.find(isValidInstallDir).get
    this.user = userDirs.find(isValidUserDir).get
    write
  }
  else {
    val source = Source.fromFile(config)
    val lines = source.getLines
    this.install = new File(lines.next)
    this.user = new File(lines.next)
    this.mods = lines.toSeq
  }
  
  private def isValidInstallDir( f: File ) =
    f != null && f.exists() && f.list.contains("Content") && f.list.contains("Mods")

  private def isValidUserDir( f: File ) =
    f != null && f.exists() && f.list.contains("Saved Designs") && f.list.contains("WIP")

  private def installDirs : Iterator[File] = Iterator(
    new File( "C:\\Program Files\\Steam\\steamapps\\common\\StarDrive"),
    new File( "C:\\Program Files (x86)\\Steam\\steamapps\\common\\StarDrive" )
  ) ++ continually(showChooser("Select StarDrive Install Directory")).flatten

  private def userDirs : Iterator[File] = Iterator(
    new File( System.getProperty("user.home") + "/AppData/Roaming/StarDrive" )
  ) ++ continually(showChooser("Select StarDrive Saved Ships Directory")).flatten
  
  private def showChooser( prompt: String ) : Iterator[File] = {
    chooser.setDialogTitle(prompt)
    val accept = chooser.showOpenDialog(null)
    if ( accept == JFileChooser.APPROVE_OPTION ) {
      val selected = chooser.getSelectedFile
      Iterator( selected, selected.getParentFile ) ++ selected.listFiles
    }
    else if ( accept == JFileChooser.CANCEL_OPTION ) {
      System.exit(0)
      Iterator.empty
    }
    else {
      Iterator.empty
    }
  }
  
  private def write = {
    val config = new File("config")
    if ( !config.exists() ) config.createNewFile
    val writer = new PrintStream( config )
    writer.println(install)
    writer.println(user)
    mods.foreach(writer.println)
    writer.close
  }
}