package gui

import java.awt.Color
import scala.swing.Label
import data.general.DataModel
import scala.swing.Panel
import scala.swing.BorderPanel
import scala.swing.Alignment
import scala.swing.BoxPanel
import scala.swing.Orientation
import data.xml.ShipModule

class ModuleStats( model: DataModel ) extends BoxPanel(Orientation.Vertical) {

  import model._

  val moduleName = addLabel
  moduleName.font = moduleName.font.deriveFont(20.0f : Float)

  val description = addLabel

  val restrictions = addLabel
  val image = addLabel
  val itemSize = addLabel
  val mass = addLabel
  val cost = addLabel
  val health = addLabel
  val power = addLabel

  val bonusRepair = addLabel

  val shieldStrength = addLabel
  val shieldSize = addLabel
  val shieldRecharge = addLabel
  val shieldDelay = addLabel

  val powerStorage = addLabel

  val engineThrust = addLabel
  val engineTurn = addLabel
  val engineWarp = addLabel

  val ordnanceCapacity = addLabel
  val cargoCapacity = addLabel

  val weaponRange = addLabel
  val weaponField = addLabel
  val weaponFireDelay = addLabel
  val weaponProjectileCount = addLabel
  val weaponProjectileSpeed = addLabel
  val weaponBeamPowerDrain = addLabel
  val weaponOrdnanceCost = addLabel
  val weaponPowerCost = addLabel

  val explodes = addLabel
  explodes.text = "Warning: This module will explode if destroyed"
  explodes.foreground = Color.RED.darker()
  explodes.visible = false


  def addLabel : Label = {
    val label = new Label
    contents += label
    label
  }

  def showIf[T]( b: Boolean, opt: Option[T])( labels: Label*)(f : T => Unit ) : Unit = {
    for ( label <- labels ) { label.visible = b }
    opt.foreach(f)
  }

  def showIf[T]( opt: Option[T] ) (labels: Label*)(f: T => Unit) : Unit =
    showIf( opt.isDefined, opt )(labels:_*)(f)

  reactions += {
    case ModuleSelected(mod) => {
      moduleName.text = token(mod.nameIndex)
      description.text = "<html>" + token(mod.descriptionIndex) + "</html>"
      restrictions.text = "Restrictions: " + mod.restrictions.description
      image.icon = moduleImage(mod).icon
      itemSize.text = "Size: " + mod.xSize + "x" + mod.ySize
      mass.text = "Mass: " + mod.mass
      cost.text = "Cost: " + mod.cost
      health.text = "Health: " + mod.health
      power.text = "Power: " + (-mod.powerDraw)

      showIf( mod.bonusRepair )( bonusRepair ) { data =>
        bonusRepair.text = "Repair Bonus: " + data
      }

      showIf( mod.moduleType == "Shield", mod.shieldData
           )( shieldSize, shieldStrength, shieldRecharge, shieldDelay ){ data =>
          shieldStrength.text = "Shield Strength: " + data.shieldPower
          shieldSize.text = "Shield Size:" + data.radius
          shieldRecharge.text = "Shield Recharge Rate: " + data.rechargeRate
          shieldDelay.text = "Shield Recharge Delay: " + data.rechargeDelay
      }

      showIf( mod.moduleType == "PowerPlant" || mod.moduleType == "FuelCell", mod.powerPlantData
           )( explodes, powerStorage ) { data =>
        power.text = "Power: " + data.powerFlowMax
        powerStorage.text = "Power Store: " + data.powerStoreMax
        explodes.visible = data.explodes
      }

      showIf( mod.moduleType == "Engine", mod.engineData
           )( engineThrust, engineTurn, engineWarp ) { data =>
        engineThrust.text = "Thrust: " + data.thrust
        engineWarp.text = "Warp: " + data.warpThrust
        engineTurn.text = "Turn: " + data.turnThrust
      }

      weaponBeamPowerDrain.visible = false
      weaponFireDelay.visible = false
      weaponOrdnanceCost.visible = false
      weaponPowerCost.visible = false
      weaponProjectileCount.visible = false
      weaponProjectileSpeed.visible = false

      showIf(mod.weaponData) (weaponField) { weap =>
        weaponField.text = "Field of Fire: " + weap.fieldOfFire
      }

      showIf(mod.ordnanceCapacity)(ordnanceCapacity) { cap =>
        ordnanceCapacity.text = "Ordnance Capacity: " + cap
      }

      showIf(mod.cargoCapacity)(cargoCapacity) { cap =>
        cargoCapacity.text = "Cargo Capacity: " + cap
      }

      showIf(mod.weaponData.map(weap => weapon(weap.weaponType))
           )( weaponRange, weaponFireDelay ) { stat =>

           showIf( stat.projectileCount )( weaponProjectileCount ) { data =>
           weaponProjectileCount.text = "Projectile Count: " + data
           }

           showIf( stat.projectileSpeed )( weaponProjectileSpeed ) { data =>
           weaponProjectileSpeed.text = "Projectile Speed: " + data
           }

           showIf( stat.beamPowerPerSec )( weaponBeamPowerDrain ) { data =>
           weaponBeamPowerDrain.text = "Power / Sec: " + data
           }

           showIf( stat.ordnancePerShot )( weaponOrdnanceCost ) { data =>
           weaponOrdnanceCost.text = "Ordnance / Shot: " + data
           }

           showIf( stat.powerPerShot )( weaponPowerCost ) { data =>
           weaponPowerCost.text = "Power / Shot: " + data
           }
       }
    }
  }
}