package data.general

import java.io.File
import java.io.PrintStream
import javax.swing.JFileChooser
import scala.Iterator.continually
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.io.Source
import scalaz.Scalaz._
import com.weiglewilczek.slf4s.Logging

object Config extends Logging {

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
    mods = mods.filterNot(_ === name)
    write
  }

  def clearMods = {
    mods = Vector()
    write
  }

  private[this] val config = new File("config")
  if ( !config.exists() || !config.canRead() ) {
    logger.info("Cannot read config file. Finding install directory...")
    this.install = installDirs.find(isValidInstallDir).get
    this.user = userDirs.find(isValidUserDir).get
    write
  }
  else {
    logger.info("Loading from config file...")
    val source = Source.fromFile(config)
    this.install = getInstallDirFromFile(source)
    this.user = getUserDirFromFile(source)
    this.mods = getModsFromFile(source)
    logger.info("Install Directory: " + install.getAbsolutePath())
    logger.info("User Directory: " + user.getAbsolutePath())
    write
  }
  
  private def getInstallDirFromFile( s: Source ) : File = {
    s.getLines.find(_.startsWith("Install:")) match {
      case Some(line) => new File( line.replaceFirst("Install:", "").trim )
      case None => installDirs.find(isValidInstallDir).get
    }
  }
  
  private def getUserDirFromFile( s: Source ) : File = {
    s.getLines.find(_.startsWith("User:")) match {
      case Some(line) => new File( line.replaceFirst("User:", "").trim )
      case None => userDirs.find(isValidUserDir).get
    }
  }
  
  private def getModsFromFile( s: Source ) : Seq[String] = {
    s.getLines.filter(_.startsWith("Mod:")).map(_.replaceFirst("Mod:", "").trim).toSeq
  }

  private def isValidInstallDir( f: File ) : Boolean = {

    def logFailed(reason: String) = logger.warn( "Invalid install dir (" + f + "): " + reason )
    if (f eq null) { logFailed(""); return false }
    if (!f.exists()) { logFailed("Doesn't Exist"); return false}
    if (!f.list.contains("Content")) { logFailed("Doesn't contain Content folder"); return false}
    if (!f.list.contains("Mods")) { logFailed("Doesn't contain Mods folder"); return false}
    return true;
  }

  private def isValidUserDir( f: File ) : Boolean = {
    def logFailed(reason: String) = logger.warn( "Invalid user dir (" + f + "): " + reason )
    if (f eq null) { logFailed(""); return false }
    if (!f.exists()) { logFailed("Doesn't Exist"); return false}
    if (!f.list.contains("Saved Designs")) { logFailed("Doesn't contain Saved Designs folder"); return false}
    if (!f.list.contains("WIP")) { logFailed("Doesn't contain WIP folder"); return false}
    return true
  }

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
    if ( accept === JFileChooser.APPROVE_OPTION ) {
      val selected = chooser.getSelectedFile
      Iterator( selected, selected.getParentFile ) ++ selected.listFiles
    }
    else if ( accept === JFileChooser.CANCEL_OPTION ) {
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
    writer.println("Install:" + install)
    writer.println("User:" + user)
    mods.foreach(str => writer.println("Mod:" + str))
    writer.close
  }
}