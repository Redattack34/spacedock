package data.xml

import java.io.File

import scala.Array.canBuildFrom

import com.codecommit.antixml.Elem
import com.codecommit.antixml.Selector.symbolToSelector
import com.codecommit.antixml.text

import data.general.FileExtension.extension2File
import data.general.FileExtension.file2Extension
import data.xml.Position.positions
import gui.Restrictions.SlotRestrictions
import gui.Restrictions.getSlotFromString

case class HullModuleSlot( pos: Position, restrictions: SlotRestrictions, slotOptions: Option[String] )
case class ThrusterZone(pos: Position, scale: Int)

case class Hull( name: String, hullId: String, role: String, race: String, iconPath: String,
    selectionGraphic: Option[String], thrusterList: Seq[ThrusterZone], modelPath: String,
    defaultAIState: String, moduleSlotList: Seq[HullModuleSlot])

object Hull extends XmlLoader[Hull] {

  private def slots(e : Elem) : Seq[HullModuleSlot] = for {
    moduleSlot <- e \ 'ModuleSlotData
    position <- positions( moduleSlot )
    restriction <- moduleSlot \ 'Restrictions \ text
    slotOptions = moduleSlot \ 'SlotOptions \ text
  } yield HullModuleSlot(position, getSlotFromString(restriction), slotOptions.headOption )

  private def thrusters(e : Elem) : Seq[ThrusterZone] = for {
    thrusterList <- e \ 'ThrusterList
    thrusterZone <- thrusterList \ 'ThrusterZone
    pos <- positions( thrusterZone )
    scale <- thrusterZone \ 'scale \ text
  } yield ThrusterZone( pos, scale.toInt )

  def load(f: Option[File], e : Elem) : Seq[Hull] = for {
    name <- e \ 'Name \ text
    hull <- e \ 'Hull \ text
    role <- e \ 'Role \ text
    iconPath <- e \ 'IconPath \ text
    selectionGraphic = e \ 'SelectionGraphic \ text
    allThrusters = thrusters( e )
    modelPath <- e \ 'ModelPath \ text
    defaultAIState <- e \ 'DefaultAIState \ text
    moduleSlotList <- e \ 'ModuleSlotList
    moduleSlots = slots(moduleSlotList)
  } yield Hull( name, hull, role, "", iconPath, selectionGraphic.headOption, allThrusters,
      modelPath, defaultAIState, moduleSlots)

  def directory(base: File) = base
  
  override def loadAll(base: File) : Seq[(File, Option[Hull])] = {
    val dir = base / 'Hulls
    
    if ( !dir.exists || !dir.canRead() ) return Seq()
    
    val hulls = for {
      race <- dir.listFiles
      ship <- race.listFiles
      loaded = super.loadFromFile(ship)
    } yield (ship, loaded.map( _.copy( race = race.getName ) ))
    
    return hulls
  }
}