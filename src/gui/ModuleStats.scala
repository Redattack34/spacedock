package gui

import java.awt.Color
import scala.swing.BoxPanel
import scala.swing.Label
import scala.swing.Orientation
import scalaz.Scalaz._
import data.general.DataModel
import java.text.DecimalFormat

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

  val singleDecimal = new DecimalFormat("#######.#")

  reactions += {
    case ModuleSelected(mod) => {
      moduleName.text = token(mod.nameIndex)
      description.text = "<html>" + token(mod.descriptionIndex) + "</html>"
      restrictions.text = "Restrictions: " + mod.restrictions.description
      image.icon = moduleImage(mod).icon
      itemSize.text = "Size: " + mod.xSize + "x" + mod.ySize
      mass.text = "Mass: " + singleDecimal.format(mod.mass)
      cost.text = "Cost: " + singleDecimal.format(mod.cost)
      health.text = "Health: " + mod.health
      power.text = "Power: " + singleDecimal.format(-mod.powerDraw)

      showIf( mod.bonusRepair )( bonusRepair ) { bonus =>
        bonusRepair.text = "Repair Bonus: " + bonus
      }

      showIf( mod.moduleType === "Shield", mod.shieldData
           )( shieldSize, shieldStrength, shieldRecharge, shieldDelay ){ shieldData =>
          shieldStrength.text = "Shield Strength: " + shieldData.shieldPower
          shieldSize.text = "Shield Size:" + shieldData.radius
          shieldRecharge.text = "Shield Recharge Rate: " + singleDecimal.format(shieldData.rechargeRate)
          shieldDelay.text = "Shield Recharge Delay: " + singleDecimal.format(shieldData.rechargeDelay)
      }

      showIf( mod.moduleType === "PowerPlant" || mod.moduleType === "FuelCell", mod.powerPlantData
           )( explodes, powerStorage ) { powerData =>
        power.text = "Power: " + powerData.powerFlowMax
        powerStorage.text = "Power Store: " + powerData.powerStoreMax
        explodes.visible = powerData.explodes
      }

      showIf( mod.moduleType === "Engine", mod.engineData
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

      showIf(mod.weaponData.flatMap(weap => weapon(weap.weaponType))
           )( weaponRange, weaponFireDelay ) { stat =>

           showIf( stat.projectileCount )( weaponProjectileCount ) { data =>
             weaponProjectileCount.text = "Projectile Count: " + data
           }

           showIf( stat.projectileSpeed )( weaponProjectileSpeed ) { data =>
             weaponProjectileSpeed.text = "Projectile Speed: " + data
           }

           showIf( stat.beamPowerPerSec )( weaponBeamPowerDrain ) { data =>
             weaponBeamPowerDrain.text = "Power / Sec: " + singleDecimal.format(data)
           }

           showIf( stat.ordnancePerShot )( weaponOrdnanceCost ) { data =>
             weaponOrdnanceCost.text = "Ordnance / Shot: " + singleDecimal.format(data)
           }

           showIf( stat.powerPerShot )( weaponPowerCost ) { data =>
             weaponPowerCost.text = "Power / Shot: " + singleDecimal.format(data)
           }
       }
    }
  }
}