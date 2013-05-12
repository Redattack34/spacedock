package data.general

import java.io.File
import java.io.PrintStream

import javax.swing.JFileChooser
import javax.swing.JOptionPane

import scala.Array.canBuildFrom
import scala.Array.fallbackCanBuildFrom
import scala.Iterator.continually
import scala.collection.TraversableOnce.flattenTraversableOnce
import scala.io.Source

import scalaz.Scalaz.ToEqualOps
import scalaz.Scalaz.intInstance
import scalaz.Scalaz.stringInstance

import com.weiglewilczek.slf4s.Logging

import data.general.FileExtension.extension2File
import data.general.FileExtension.file2Extension

object Config extends Logging {

  private final val INSTALL_PREFIX = "Install:"
  private final val USER_PREFIX = "User:"
  private final val LANGUAGE_PREFIX = "Language:"
  private final val MOD_PREFIX = "Mod:"

  private[this] var _install : File = null
  def install : File = _install
  private def install_=(file: File) = {
    _install = file
  }

  private[this] var _user : File = null
  def user : File = _user
  private def user_=(file: File) = {
    _user = file
  }

  private[this] var _mods : Seq[String] = Vector()
  def mods : Seq[String] = _mods
  private def mods_=(newMods: Seq[String]) = {
    _mods = newMods
  }

  private[this] var _language : String = null
  def language : String = _language;
  private def language_=(newLang: String) = {
    _language = newLang
  }

  val chooser = new JFileChooser
  chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)

  def addMod( name: String ) : Unit = {
    mods = mods ++ Seq(name)
    write
  }

  def removeMod( name: String ) : Unit = {
    mods = mods.filterNot(_ === name)
    write
  }

  def clearMods() : Unit = {
    mods = Vector()
    write
  }

  private[this] val config = new File("config")
  if ( !config.exists() || !config.canRead() ) {
    logger.info("Cannot read config file. Finding install directory...")
    this.install = installDirs.find(isValidInstallDir).get
    this.user = userDirs.find(isValidUserDir).get
    this.language = getLanguage()
  }
  else {
    logger.info("Loading from config file...")
    this.install = getStringFromFile(config, INSTALL_PREFIX)
                    .map(new File(_))
                    .orElse(installDirs.find(isValidInstallDir))
                    .get
    this.user = getStringFromFile(config, USER_PREFIX)
                  .map(new File(_))
                  .orElse(userDirs.find(isValidUserDir))
                  .get
    this.language = getStringFromFile( config, LANGUAGE_PREFIX )
                    .getOrElse("English")
    this.mods = getModsFromFile(config)
  }
  logger.info("Install Directory: " + install.getAbsolutePath())
  logger.info("User Directory: " + user.getAbsolutePath())
  logger.info("Language: " + language )
  mods.foreach( mod => logger.info("Mod: " + mod ) )
  write

  private def getStringFromFile( f: File, prefix: String ) =
    Source.fromFile(f)
      .getLines
      .find(_.startsWith(prefix))
      .map(_.replaceFirst(prefix, "").trim)

  private def getModsFromFile( f: File ) : Seq[String] = {
    Source.fromFile(f).getLines.filter(_.startsWith("Mod:")).map(_.replaceFirst("Mod:", "").trim).toSeq
  }

  private def getPossibleLanguages() : Seq[String] = {
    (install / 'Content / 'Localization).listFiles.map(_.getName).map(_.replaceAll(".xml", ""))
  }

  private def isValidInstallDir( f: File ) : Boolean = {
    def logFailed(reason: String) : Unit =
      logger.warn( "Invalid install dir (" + f + "): " + reason )

    if (f eq null) { logFailed(""); false }
    else if (!f.exists()) { logFailed("Doesn't Exist");  false}
    else if (!f.list.contains("Content")) {
      logFailed("Doesn't contain Content folder")
      false
    }
    else if (!f.list.contains("Mods")) {
      logFailed("Doesn't contain Mods folder")
      false
    }
    else true;
  }

  private def isValidUserDir( f: File ) : Boolean = {
    def logFailed(reason: String) : Unit =
      logger.warn( "Invalid user dir (" + f + "): " + reason )
    if (f eq null) { logFailed(""); false }
    else if (!f.exists()) { logFailed("Doesn't Exist"); false}
    else if (!f.list.contains("Saved Designs")) {
      logFailed("Doesn't contain Saved Designs folder")
      false
    }
    else if (!f.list.contains("WIP")) {
      logFailed("Doesn't contain WIP folder")
      false
    }
    else { true }
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

  private def getLanguage() : String = {
    val languageOptions = getPossibleLanguages
    if ( languageOptions.length == 1 ) { languageOptions.head }
    else {
      val selected = JOptionPane.showInputDialog(null, "Please select a language:",
          "Spacedock: Language Selection", JOptionPane.PLAIN_MESSAGE, null,
          languageOptions.toArray, "English")
      if ( selected == null ) { System.exit(0); "" }
      else { selected.toString }
    }
  }

  private def write = {
    if ( !config.exists() ) config.createNewFile
    val writer = new PrintStream( config )
    writer.println("Install:" + install)
    writer.println("User:" + user)
    writer.println("Language:" + language)
    mods.foreach(str => writer.println("Mod:" + str))
    writer.close
  }
}