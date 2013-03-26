package gui

import scala.swing.BoxPanel
import scala.swing.Label
import scala.swing.Orientation
import scala.collection._
import data.general.Point
import data.xml.Hull.HullModuleSlot
import java.text.DecimalFormat

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
  
  val warpSpeedFormat = new DecimalFormat("###.#k")
  val turnRateFormat = new DecimalFormat("###.##")

  reactions += {
    case ShipModelChanged(newModel) => {
      cost.text = "Production Cost: " + newModel.cost
      powerCapacity.text = "Power Capacity: " + newModel.powerCapacity
      recharge.text = "Power Recharge: " + newModel.recharge
      rechargeAtWarp.text = "Recharge at Warp: " + newModel.rechargeAtWarp
      hitpoints.text = "Total Hitpoints: " + newModel.hitpoints
      shieldPower.text = "Total Shield Power: " + newModel.shieldPower
      mass.text = "Total Mass: " + newModel.mass
      sublightSpeed.text = "Sub-Light Speed: " + newModel.sublightSpeed
      ftlSpeed.text = "FTL Speed: " + warpSpeedFormat.format( newModel.ftlSpeed.toFloat / 1000 )
      turnRate.text = "Turn Rate: " + turnRateFormat.format( newModel.turnRate )
      ordnanceCapacity.text = "Ordnance Capacity: " + newModel.ordnanceCapacity
      cargoSpace.text = "Cargo Space: " + newModel.cargoSpace
      hasBridge.text = "Has Cockpit/Bridge/CIC: " + newModel.hasCommandModule
      emptySlots.text = "Has Empty Slots: " + newModel.hasEmptySlots
    }
  }
}