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

  reactions += {
    case ShipModelChanged(newModel) => {
      import newModel._
      cost.text = "Production Cost: " + calculateCost
      powerCapacity.text = "Power Capacity: " + calculatePowerCapacity
      recharge.text = "Power Recharge: " + calculateRecharge
      rechargeAtWarp.text = "Recharge at Warp: " + calculateRechargeAtWarp
      hitpoints.text = "Total Hitpoints: " + calculateHitpoints
      shieldPower.text = "Total Shield Power: " + calculateShieldPower
      mass.text = "Total Mass: " + calculateMass
      sublightSpeed.text = "Sub-Light Speed: " + calculateSublightSpeed
      ftlSpeed.text = "FTL Speed: " + calculateFtlSpeed
      turnRate.text = "Turn Rate: " + calculateTurnRate
      ordnanceCapacity.text = "Ordnance Capacity: " + calculateOrdnanceCapacity
      cargoSpace.text = "Cargo Space: " + calculateCargoSpace
      hasBridge.text = "Has Cockpit/Bridge/CIC: " + hasCommandModule
      emptySlots.text = "Has Empty Slots: " + hasEmptySlots
    }
  }
}