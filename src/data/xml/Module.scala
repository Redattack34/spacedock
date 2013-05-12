package data.xml

import java.io.File

import com.codecommit.antixml.Elem
import com.codecommit.antixml.Selector.symbolToSelector
import com.codecommit.antixml.text

import data.general.FileExtension.extension2File
import data.general.FileExtension.file2Extension
import gui.Restrictions.ModIOE
import gui.Restrictions.ModuleRestrictions
import gui.Restrictions.getModuleFromString

case class ShieldData(shieldPower: Int, rechargeDelay: Float, rechargeRate: Float, radius: Int)

case class PowerPlantData(explodes: Boolean, powerFlowMax: Int, powerStoreMax: Int, powerRadius: Int)

case class WeaponData(weaponType: String, fieldOfFire: Int)

case class EngineData(thrust: Int, turnThrust: Int, warpThrust: Int )

case class HangarData( timerConstant: Int, isTroopBay: Boolean, isSupplyBay: Boolean )

case class ShipModule(
    //Basics
    nameIndex: Int, descriptionIndex: Int, uid: String,
    
    //Module structure stuff
    xSize: Int, ySize: Int, moduleType: String, restrictions: ModuleRestrictions,
    iconTexturePath: String,
    
    //Game stats
    cost: Float, mass: Float, health: Int, powerDraw: Float, bonusRepair: Option[Int],
    ordnanceCapacity: Option[Int], ordnanceAddedPerSecond: Option[Double], cargoCapacity: Option[Int],
    
    shieldData: Option[ShieldData],
    powerPlantData: Option[PowerPlantData],
    weaponData: Option[WeaponData],
    engineData: Option[EngineData],
    hangarData: Option[HangarData])
    
object Module extends XmlLoader[ShipModule] {

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

  def load(f: Option[File], e: Elem) : Seq[ShipModule] = for {
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
      ordnanceAddedPerSecond = e \ 'OrdnanceAddedPerSecond \ text
      cargoCapacity = e \ 'Cargo_Capacity \ text

      shieldData = shields(e)
      powerPlantData = powerPlants(e)
      weaponData = weapons(e)
      engineData = engines(e)
      hangarData = hangars(e)
    } yield ShipModule( name.toInt, description.toInt, uid,
        xSize.headOption.map(_.toInt).getOrElse(1),
        ySize.headOption.map(_.toInt).getOrElse(1), moduleType,
        getModuleFromString(restrictions), iconTexturePath, cost.toFloat,
        mass.toFloat, health.toInt,
        powerDraw.headOption.map(_.toFloat).getOrElse(0.0f),
        bonusRepair.headOption.map(_.toInt),
        ordnanceCapacity.headOption.map(_.toInt),
        ordnanceAddedPerSecond.headOption.map(_.toDouble),
        cargoCapacity.headOption.map(_.toInt),
        shieldData.headOption, powerPlantData.headOption,
        weaponData.headOption, engineData.headOption, hangarData.headOption )

  def directory(base: File) = base / 'ShipModules
  
  val dummy = new ShipModule(0, 0, "Dummy", 1, 1, "", ModIOE, "", 0.0f, 0, 0, 0,
      None, None, None, None, None, None, None, None, None)
}