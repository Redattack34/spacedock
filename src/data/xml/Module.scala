package data.xml

import java.io.File

import scala.Array.canBuildFrom

import com.codecommit.antixml.Elem
import com.codecommit.antixml.Selector.symbolToSelector
import com.codecommit.antixml.XML
import com.codecommit.antixml.text

import data.general.FileExtension._

case class ShieldData(shieldPower: Int, rechargeDelay: Float, rechargeRate: Float, radius: Int)

case class PowerPlantData(explodes: Boolean, powerFlowMax: Int, powerStoreMax: Int, powerRadius: Int)

case class WeaponData(weaponType: String, fieldOfFire: Int)

case class EngineData(thrust: Int, turnThrust: Int, warpThrust: Int )

case class HangarData( timerConstant: Int, isTroopBay: Boolean, isSupplyBay: Boolean )

case class ShipModule(
		//Basics
		nameIndex: Int, descriptionIndex: Int, uid: String,
		
		//Module structure stuff
		xSize: Int, ySize: Int, moduleType: String, restrictions: String,
		iconTexturePath: String,
		
		//Game stats
		cost: Float, mass: Float, health: Int, powerDraw: Float, bonusRepair: Option[Int],
		ordnanceCapacity: Option[Int], cargoCapacity: Option[Int],
		
		shieldData: Option[ShieldData],
		powerPlantData: Option[PowerPlantData],
		weaponData: Option[WeaponData],
		engineData: Option[EngineData],
		hangarData: Option[HangarData])
		
object Module {

  private def shields( e : Elem ) : Seq[ShieldData] = for {
    shieldPower <- e \ 'shield_power \ text
    rechargeDelay <- e \ 'shield_recharge_delay \ text
    rechargeRate <- e \ 'shield_recharge_rate \ text
    radius <- e \ 'shield_radius \ text
  } yield ShieldData( shieldPower.toInt, rechargeDelay.toFloat,
      rechargeRate.toFloat, radius.toInt )

  private def powerPlants( e : Elem ) : Seq[PowerPlantData] = for {
    explodes <- e \ 'explodes \ text
    powerFlowMax <- e \ 'PowerFlowMax \ text
    powerStoreMax <- e \ 'PowerStoreMax \ text
    powerRadius = e \ 'PowerRadius \ text
  } yield PowerPlantData( explodes.toBoolean, powerFlowMax.toInt,
      powerStoreMax.toInt, powerRadius.headOption.map(_.toInt).getOrElse(0))

  private def weapons( e : Elem ) : Seq[WeaponData] = for {
    weaponType <- e \ 'WeaponType \ text
    fieldOfFire <- e \ 'FieldOfFire \ text
  } yield WeaponData( weaponType, fieldOfFire.toInt )

  private def engines( e: Elem ) : Seq[EngineData] = for {
    thrust <- e \ 'thrust\ text
    turnThrust <- e \ 'TurnThrust \ text
    warpThrust <- e \ 'WarpThrust \ text
  } yield EngineData( thrust.toInt, turnThrust.toInt, warpThrust.toInt )

  private def hangars( e: Elem ) : Seq[HangarData] = for {
    timer <- e \ 'hangerTimerConstant \ text
    troopBay = e \ 'IsTroopBay \ text
    supplyBay = e \ 'IsSupplyBay \ text
  } yield HangarData( timer.toInt,
      troopBay.headOption.map(_.toBoolean).getOrElse(false),
      supplyBay.headOption.map(_.toBoolean).getOrElse(false))

  private def modules( e: Elem ) : Seq[ShipModule] = for {
      name <- e \ 'NameIndex \ text
      description <- e \ 'DescriptionIndex \ text
      uid <- e \ 'UID \ text

      xSize = e \ 'XSIZE \ text
      ySize = e \ 'YSIZE \ text
      moduleType <- e \ 'ModuleType \ text
      restrictions <- e \ 'Restrictions \ text
      iconTexturePath <- e \ 'IconTexturePath \ text

      cost <- e \ 'Cost \ text
      mass <- e \ 'Mass \ text
      health <- e \ 'Health \ text
      powerDraw = e \ 'PowerDraw \ text
      bonusRepair = e \ 'BonusRepairRate \ text
      ordnanceCapacity = e \ 'OrdinanceCapacity \ text
      cargoCapacity = e \ 'Cargo_Capacity \ text

      shieldData = shields(e)
      powerPlantData = powerPlants(e)
      weaponData = weapons(e)
      engineData = engines(e)
      hangarData = hangars(e)
    } yield ShipModule( name.toInt, description.toInt, uid,
        xSize.headOption.map(_.toInt).getOrElse(1),
        ySize.headOption.map(_.toInt).getOrElse(1), moduleType,
        restrictions, iconTexturePath, cost.toFloat, mass.toFloat, health.toInt,
        powerDraw.headOption.map(_.toFloat).getOrElse(0.0f),
        bonusRepair.headOption.map(_.toInt),
        ordnanceCapacity.headOption.map(_.toInt),
        cargoCapacity.headOption.map(_.toInt),
        shieldData.headOption, powerPlantData.headOption,
        weaponData.headOption, engineData.headOption, hangarData.headOption )


  def loadModules( base: File ) : Seq[(File, Option[ShipModule])] = {
    val modulesDir = base / 'ShipModules
    
    if ( !modulesDir.exists ) return Seq()
    
    val allModules = for {
        file <- modulesDir.listFiles().toSeq.par
        xml = XML.fromInputStream(XmlUtils.read(file))
        module = modules(xml)
    } yield (file, module.headOption)
    allModules.seq
  }

  val dummy = new ShipModule(0, 0, "Dummy", 1, 1, "", "", "", 0.0f, 0, 0, 0,
      None, None, None, None, None, None, None, None)

  def main(args: Array[String]) {
    val f = new File("C:\\Program Files (x86)\\Steam\\steamapps\\common\\StarDrive\\Content\\ShipModules")
    val allModules = for {
      file <- f.listFiles()
      xml = XML.fromInputStream(XmlUtils.read(file))
      module = modules(xml)
    } yield (file.getName, module.headOption)

    allModules.filter(_._2.isEmpty).foreach(f => println("Failed to parse: " + f._1))
    allModules.filter(_._1 == "Dummy").foreach(println)
  }
}