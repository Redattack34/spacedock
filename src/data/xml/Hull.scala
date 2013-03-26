package data.xml

import java.io.File
import com.codecommit.antixml.Elem
import com.codecommit.antixml.Selector.symbolToSelector
import com.codecommit.antixml.XML
import com.codecommit.antixml.text
import scala.collection.immutable.HashMap
import data.xml.Position._

object Hull {



  case class HullModuleSlot( pos: Position, restrictions: String,
      slotOptions: Option[String], shieldPower: Option[Int], facing: Option[Float] )

  private def slots(e : Elem) : Seq[HullModuleSlot] = for {
    moduleSlot <- e \ 'ModuleSlotData
    position <- positions( moduleSlot )
    restriction <- moduleSlot \ 'Restrictions \ text
    slotOptions = moduleSlot \ 'SlotOptions \ text
    shieldPower = moduleSlot \ 'Shield_Power \ text
    facing = moduleSlot \ 'facing \ text
  } yield HullModuleSlot(position, restriction,
      slotOptions.headOption, shieldPower.headOption.map(_.toInt),
      facing.headOption.map(_.toFloat) )

  case class ThrusterZone(pos: Position, scale: Int)

  private def thrusters(e : Elem) : Seq[ThrusterZone] = for {
    thrusterList <- e \ 'ThrusterList
    thrusterZone <- thrusterList \ 'ThrusterZone
    pos <- positions( thrusterZone )
    scale <- thrusterZone \ 'scale \ text
  } yield ThrusterZone( pos, scale.toInt )

  case class Hull( name: String, hullId: String, role: String, race: String, iconPath: String,
      selectionGraphic: Option[String], thrusterList: Seq[ThrusterZone], modelPath: String,
      defaultAIState: String, moduleSlotList: Seq[HullModuleSlot])

  def hulls(race: String, hullId: String, e : Elem) : Seq[Hull] = for {
    name <- e \ 'Name \ text
    role <- e \ 'Role \ text
    iconPath <- e \ 'IconPath \ text
    selectionGraphic = e \ 'SelectionGraphic \ text
    allThrusters = thrusters( e )
    modelPath <- e \ 'ModelPath \ text
    defaultAIState <- e \ 'DefaultAIState \ text
    moduleSlotList <- e \ 'ModuleSlotList
    moduleSlots = slots(moduleSlotList)
  } yield Hull( name, hullId, role, race, iconPath, selectionGraphic.headOption, allThrusters,
      modelPath, defaultAIState, moduleSlots)

  private def raceHulls( dir: File ) : Seq[(File, Option[Hull])] = {
    val raceHulls = for {
      file <- dir.listFiles().toSeq.par
      xml = XML.fromInputStream(XmlUtils.read(file))
      hull = hulls(dir.getName, file.getName.replace(".xml", ""), xml)
    } yield (file, hull.headOption)
    raceHulls.seq
  }

  def loadHulls( base: File ) : Seq[(File, Option[Hull])] = {
    val hullsDir = new File(base.getAbsolutePath() + "/Content/Hulls")
    hullsDir.listFiles().flatMap(raceHulls)
  }

  def main(args: Array[String]) {
    val f = new File("C:\\Program Files (x86)\\Steam\\steamapps\\common\\StarDrive\\Content\\Hulls")
    val allHulls = for {
      dir <- f.listFiles()
      file <- dir.listFiles()
      xml = XML.fromInputStream(XmlUtils.read(file))
      hull = hulls(dir.getName, file.getName.replace(".xml", ""), xml)
    } yield (file.getName, hull.headOption)

    allHulls.filter(_._2.isEmpty).foreach(f => println("Failed to parse: " + f._1))
  }
}