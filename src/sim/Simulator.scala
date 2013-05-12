package sim

import scalaz.syntax.order.ToOrderOps
import data.general.DataModel
import data.xml.{Weapon => ShipWeapon}
import gui.ShipModel
import scalaz.Order
import com.weiglewilczek.slf4s.Logging

trait ShipStatisticsOut {
    def emitDamage( damage: Double, empDamage: Double, direction: Double ) : Unit
    def frameComplete(currentEnergy: Double, currentOrdnance: Double, percent: Double) : Unit
}

object Simulator {

  private val FRAMES_PER_SECOND = 60

  private def clamp[T : Order]( min: T, cur: T, max: T) : T = {
    if ( cur lt min ) min
    else if ( cur gt max ) max
    else cur
  }

  private trait WeaponStatisticsOut {
    def emitDamage( damage: Double, empDamage: Double, direction: Double ) : Unit
    def emitPowerChange( power: Double ) : Unit
    def emitOrdnanceChange( ordnance: Double ) : Unit
  }

  private class Simulation( val maxPower: Int, val maxOrdnance: Int,
      val powerRegenPerFrame: Double, val ordnanceRegenPerFrame: Double,
      val weapons: Traversable[Weapon],
      out: ShipStatisticsOut ) extends WeaponStatisticsOut with Logging {
    private[this] var currentPower: Double = maxPower
    private[this] var currentOrdnance: Double = maxOrdnance

    override def emitDamage( damage: Double, empDamage: Double, direction: Double ) : Unit =
      out.emitDamage(damage, empDamage, direction)

    override def emitPowerChange( power: Double ) : Unit = {
      logger.info( "Power Change: " + power )
      currentPower = clamp( 0.0, currentPower + power, maxPower );
    }

    override def emitOrdnanceChange( ordnance: Double ) : Unit = {
      logger.info( "Ordnance Change: " + ordnance )
      currentOrdnance = clamp( 0.0, currentOrdnance + ordnance, maxOrdnance )
    }

    def simulate( seconds: Int ) : Unit = {
      val numFrames = seconds * FRAMES_PER_SECOND
      var currentFrame = 0
      for {
        second <- 0 until seconds
        frame <- 0 until FRAMES_PER_SECOND
      } {
        for {
          weapon <- weapons
        } {
          weapon.runOneFrame(currentPower, currentOrdnance, this)
        }
        emitPowerChange( +powerRegenPerFrame )
        emitOrdnanceChange( +ordnanceRegenPerFrame )
        currentFrame += 1
        out.frameComplete(currentPower, currentOrdnance, currentFrame.toDouble / numFrames)
      }
    }
  }

  private type Step = (Double, Double, WeaponStatisticsOut) => WeaponState
  private sealed trait WeaponState {
    def runFrame( power: Double, ordnance: Double, out: WeaponStatisticsOut ) : WeaponState
  }
  private case class Ready( val f: Step ) extends WeaponState {
    def runFrame(power, ordnance, out) : WeaponState = f( power, ordnance, out)
  }
  private case class Reloading( frames : Int, f: Step ) extends WeaponState {
    private var framesRemaining: Int = frames
    def runFrame(power, ordnance, out) : WeaponState = {
      if ( framesRemaining <= 2 ) Ready(f)
      else {
        framesRemaining = framesRemaining - 1
        this
      }
    }
  }

  private def salvo(salvoCount: Int, salvoFrames: Int, delayFrames: Int,
      powerCost: Double, ordnanceCost: Double, fire: (WeaponStatisticsOut) => Unit) : WeaponState = {

    def step(remainingSalvos: Int)(power: Double, ordnance: Double, out: WeaponStatisticsOut) : WeaponState = {
      if ( power >= powerCost && ordnance >= ordnanceCost ) {
        out.emitPowerChange(-powerCost)
        out.emitOrdnanceChange(-ordnanceCost)
        fire(out)
        if ( remainingSalvos == 0 ) {
          Reloading(delayFrames, step(salvoCount))
        }
        else {
          Reloading(salvoFrames, step(remainingSalvos - 1))
        }
      }
      else {
        if ( remainingSalvos == 0 ) {
          Ready(step(salvoCount))
        }
        else {
          Reloading(delayFrames, step(salvoCount))
        }
      }
    }
    Ready(step(salvoCount))
  }

  private val BEAM_TICKS = 180
  private def beam( delayFrames: Int, powerCostPerFrame: Double,
      fire: (WeaponStatisticsOut) => Unit) : WeaponState = {
    def step(remainingTicks: Int)(power: Double, ordnance: Double, out: WeaponStatisticsOut) : WeaponState = {
      if ( power >= powerCostPerFrame ) {
        out.emitPowerChange(-powerCostPerFrame)
        fire(out)
        if ( remainingTicks == 0 ) {
          Reloading(delayFrames, step(BEAM_TICKS))
        }
        else {
          Ready(step(remainingTicks - 1))
        }
      }
      else {
        if ( remainingTicks == 0 ) {
          Ready(step(BEAM_TICKS))
        }
        else {
          Reloading(delayFrames, step(BEAM_TICKS))
        }
      }
    }
    Ready(step(BEAM_TICKS))
  }


  private def projectileWeapon(delayFrames: Int, powerCost: Double,
      ordnanceCost: Double, fire: (WeaponStatisticsOut) => Unit) : WeaponState = {

    def step(power: Double, ordnance: Double, out: WeaponStatisticsOut) : WeaponState = {
      if ( power >= powerCost && ordnance >= ordnanceCost ) {
        out.emitPowerChange(-powerCost)
        out.emitOrdnanceChange(-ordnanceCost)
        fire(out)
        Reloading(delayFrames, step)
      }
      else {
        Ready(step)
      }
    }
    Ready(step)
  }

  private def singleShot(damage: Double, empDamage: Double, direction: Double)(out: WeaponStatisticsOut) : Unit = {
    out.emitDamage(damage, empDamage, direction)
  }

  private def multiShot(shots: Int, f: (WeaponStatisticsOut => Unit))(out: WeaponStatisticsOut) : Unit = {
    (0 until shots).foreach{ a =>
      f(out)
    }
  }

  private class Weapon( start: WeaponState ) {
    private[this] var state = start

    def runOneFrame( currentPower: Double, currentOrdnance: Double, out: WeaponStatisticsOut ) : Unit =
      state = state.runFrame(currentPower, currentOrdnance, out)
  }

  def runSimulator( model: ShipModel, data: DataModel, out: ShipStatisticsOut, seconds: Int ) : Unit = {
    val maxEnergy = model.powerCapacity
    val maxOrdnance = model.ordnanceCapacity
    val energyRegen = model.recharge / FRAMES_PER_SECOND
    val ordnanceRegen = model.ordnanceGeneration / FRAMES_PER_SECOND
    val weapons = model.allWeapons.values.map{
      case (dir, weap) =>
        getWeapon(dir, weap.weaponData.flatMap( weap => data.weapon(weap.weaponType)).get)
    }
    val simulation = new Simulation( maxEnergy, maxOrdnance, energyRegen, ordnanceRegen, weapons, out )
    simulation.simulate(seconds)
  }

  private def getWeapon( direction: Double, weap: ShipWeapon ) : Weapon = {
    val shot = singleShot(weap.damage, weap.empDamage, direction) _
    val shots = weap.projectileCount.map(a => multiShot(a, shot) _).getOrElse(shot)
    val delayFrames = toFrames(weap.fireDelay)
    val powerCost = weap.powerPerShot.getOrElse(0.0d)
    val ordnanceCost = weap.ordnancePerShot.getOrElse(0.0d)
    val state = weap.salvoCount match {
      case Some(count) => salvo(count, toFrames(weap.salvoTime.get),
              delayFrames, powerCost, ordnanceCost, shots)
      case None => if ( weap.isBeam ) beam( delayFrames,
                      weap.beamPowerPerSec.getOrElse(0.0) / FRAMES_PER_SECOND, shots )
                  else projectileWeapon( delayFrames, powerCost, ordnanceCost, shots )

    }
    new Weapon(state)
  }

  private def toFrames( time: Double ) : Int = (time * FRAMES_PER_SECOND).toInt
}
