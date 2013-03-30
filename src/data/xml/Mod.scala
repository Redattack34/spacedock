package data.xml

import com.codecommit.antixml.Elem
import com.codecommit.antixml.Selector.symbolToSelector
import com.codecommit.antixml.XML
import com.codecommit.antixml.text
import java.io.File

case class Mod(name: String, desc: String)

object Mod {

  private def mods( e: Elem ) : Seq[Mod] = for {
    name <- e \ 'ModName \ text
    desc <- e \ 'ModDescription \ text
  } yield Mod(name, desc)
  
  def loadMods( base: File ) : Seq[(File, Option[Mod])] = {
    val modsDir = new File(base.getAbsolutePath() + "/Mods")
    val allMods = for {
      file <- modsDir.listFiles().toSeq.par
      if (file.isFile())
      xml = XML.fromInputStream(XmlUtils.read(file))
      mod = mods(xml)
    } yield (file, mod.headOption)
    allMods.seq
  }
}