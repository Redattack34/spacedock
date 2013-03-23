package gui

import scala.swing.BoxPanel
import scala.swing.Label
import scala.swing.Orientation
import scala.collection._
import data.general.Point
import data.xml.Hull.HullModuleSlot

class ShipStats extends BoxPanel(Orientation.Vertical) {

  val cost = addLabel
  val powerCapacity = addLabel
  val recharge = addLabel
  val rechargeAtWarp = addLabel
  val hitpoints = addLabel
  val shieldPower = addLabel
  val mass = addLabel
  val sublightSpeed = addLabel
  val ftlSpeed = addLabel
  val turnRate = addLabel
  val ordnanceCapacity = addLabel
  val cargoSpace = addLabel

  val hasBridge = addLabel
  val emptySlots = addLabel

  def addLabel : Label = {
    val label = new Label
    contents += label
    label
  }

  def calculateCost(newModel: ShipModel) = newModel.allModules.values.map(_.cost).sum

  def calculatePowerCapacity(newModel: ShipModel) = newModel.allModules.values
      .filter(_.powerPlantData.isDefined)
      .map(_.powerPlantData.get.powerStoreMax)
      .sum

  def calculateRecharge(newModel: ShipModel) = {
    val modules = newModel.allModules.values
    val powerGeneration = modules.filter(_.powerPlantData.isDefined).map(_.powerPlantData.get.powerFlowMax).sum
    val powerUse = modules.map(_.powerDraw).sum
    powerGeneration - powerUse
  }

  def calculateRechargeAtWarp(newModel: ShipModel) = "???"

  def calculateHitpoints(newModel: ShipModel) = newModel.allModules.values.map(_.health).sum

  def calculateShieldPower(newModel: ShipModel) = newModel.allModules.values
      .filter(_.shieldData.isDefined)
      .map(_.shieldData.get.shieldPower)
      .sum

  def calculateMass(newModel: ShipModel) = newModel.allModules.values.map(_.mass).sum

  def calculateSublightSpeed(newModel: ShipModel ) = "???"

  def calculateFtlSpeed(newModel: ShipModel ) = "???"

  def calculateTurnRate(newModel: ShipModel) = "???"

  def calculateOrdnanceCapacity(newModel: ShipModel) = newModel.allModules.values
      .flatMap(_.ordnanceCapacity)
      .sum

  def calculateCargoSpace(newModel: ShipModel) = newModel.allModules.values
      .flatMap(_.cargoCapacity)
      .sum

  def hasCommandModule(newModel: ShipModel) = newModel.allModules.values
      .find(_.moduleType == "Command")
      .isDefined

  def hasEmptySlots(newModel: ShipModel) = {
    val slots = mutable.Map[Point, HullModuleSlot](newModel.allSlots.toSeq:_*)

    newModel.allModules.foreach { tuple =>
      val (point, module) = tuple

      for {
        x <- point.x until point.x + module.xSize
        y <- point.y until point.y + module.ySize
      } slots -= Point(x, y)
    }

    !slots.isEmpty
  }

  reactions += {
    case ShipModelChanged(newModel) => {
      cost.text = "Production Cost: " + calculateCost(newModel)
      powerCapacity.text = "Power Capacity: " + calculatePowerCapacity(newModel)
      recharge.text = "Power Recharge: " + calculateRecharge(newModel)
      rechargeAtWarp.text = "Recharge at Warp: " + calculateRechargeAtWarp(newModel)
      hitpoints.text = "Total Hitpoints: " + calculateHitpoints(newModel)
      shieldPower.text = "Total Shield Power: " + calculateShieldPower(newModel)
      mass.text = "Total Mass: " + calculateMass(newModel)
      sublightSpeed.text = "Sub-Light Speed: " + calculateSublightSpeed(newModel)
      ftlSpeed.text = "FTL Speed: " + calculateFtlSpeed(newModel)
      turnRate.text = "Turn Rate: " + calculateTurnRate(newModel)
      ordnanceCapacity.text = "Ordnance Capacity: " + calculateOrdnanceCapacity(newModel)
      cargoSpace.text = "Cargo Space: " + calculateCargoSpace(newModel)
      hasBridge.text = "Has Cockpit/Bridge/CIC: " + hasCommandModule(newModel)
      emptySlots.text = "Has Empty Slots: " + hasEmptySlots(newModel)
    }
  }
}