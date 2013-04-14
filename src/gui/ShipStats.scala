package gui

import java.text.DecimalFormat

import scala.Option.option2Iterable
import scala.swing.Alignment
import scala.swing.BoxPanel
import scala.swing.Label
import scala.swing.Orientation

import data.general.DataModel

class ShipStats(dataModel: DataModel) extends BoxPanel(Orientation.Vertical) {

  val cost = addLabel
  val upkeep = addLabel
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
  val research = addLabel

  def addLabel : Label = {
    val label = new Label
    label.horizontalAlignment = Alignment.Left
    label.horizontalTextPosition = Alignment.Left
    contents += label
    label
  }

  val warpSpeedFormat = new DecimalFormat("####.#k")
  val twoDecimals = new DecimalFormat("####.##")
  val singleDecimal = new DecimalFormat("####.#")

  def getResearch(model: ShipModel) : String = {
    val hullResearch = dataModel.techForHull(model.hull.race, model.hull.hullId)
    val modResearch = dataModel.techsForMods(model.allModules.values.toSet)
    val allResearch = hullResearch ++ modResearch
    allResearch.map(_.nameID).map(dataModel.token).mkString(", ")
  }

  reactions += {
    case ShipModelChanged(newModel) => {
      cost.text = "Production Cost: " + newModel.cost.toInt
      upkeep.text = "Upkeep: " + twoDecimals.format( newModel.upkeep ) + "/turn"
      powerCapacity.text = "Power Capacity: " + newModel.powerCapacity
      recharge.text = "Power Recharge: " + newModel.recharge.toInt
      rechargeAtWarp.text = "Recharge at Warp: " + newModel.rechargeAtWarp.toInt
      hitpoints.text = "Total Hitpoints: " + newModel.hitpoints
      shieldPower.text = "Total Shield Power: " + newModel.shieldPower
      mass.text = "Total Mass: " + newModel.mass.toInt
      sublightSpeed.text = "Sub-Light Speed: " + singleDecimal.format( newModel.sublightSpeed )
      ftlSpeed.text = "FTL Speed: " + warpSpeedFormat.format( newModel.ftlSpeed.toFloat / 1000 )
      turnRate.text = "Turn Rate: " + twoDecimals.format( newModel.turnRate )
      ordnanceCapacity.text = "Ordnance Capacity: " + newModel.ordnanceCapacity
      cargoSpace.text = "Cargo Space: " + newModel.cargoSpace
      hasBridge.text = "Has Cockpit/Bridge/CIC: " + newModel.hasCommandModule
      emptySlots.text = "Has Empty Slots: " + newModel.hasEmptySlots
      research.text = "<html>Research Required: " + getResearch(newModel) + "</html>"
    }
  }
}