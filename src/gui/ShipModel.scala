package gui

import data.xml.Hull._
import data.xml.Module._
import data.xml.Ship._
import data.general.DataModel
import data.xml.Module._
import data.general.Point
import data.general.RangeOverlap._
import scala.collection._

case class ModelSlot( hullSlot: HullModuleSlot, module: ShipModule, power: Boolean, facing: Float, slotOption: Option[String] )

object ShipModel {

  private val emptyHull : Hull = Hull("", "", "", "", "", None, Seq(), "", "", Seq() )
  private val emptyShip : Ship = Ship("", "", None, None, "", "", Seq() )

  val empty = apply(null, emptyHull, emptyShip)

  def apply( dataModel : DataModel, hull: Hull, ship : Ship ) : ShipModel = {

    val hullSlots = hull.moduleSlotList.map( slot => (slot.pos, slot)).toMap
    val shipModules = ship.moduleSlotList.map( slot => (slot.pos, slot)).toMap
    val modelSlots = hullSlots.mapValues{ slot =>
      val shipSlot = shipModules.get(slot.pos)
      val module = shipSlot.map(_.installed) match {
        case Some(x) if x != "Dummy" => dataModel.module(x)
        case _ => dummy
      }

      ModelSlot( slot, module, false, shipSlot.map(_.facing).getOrElse(0.0f),
          shipSlot.flatMap(_.slotOptions) )
    }

    val pointSlots = modelSlots.map( tuple => ( Point(tuple._1.x / 16, tuple._1.y / 16), tuple._2) )

    val minSlots = if ( pointSlots.isEmpty ) pointSlots
                   else {
                     val minX = pointSlots.keys.minBy(_.x).x
                     val minY = pointSlots.keys.minBy(_.y).y

                     pointSlots.map( tuple => ( Point(tuple._1.x - minX, tuple._1.y - minY), tuple._2 ) )
                   }

    new ShipModel( hull, ship, ship.combatState.getOrElse("AttackRuns"), minSlots ).computePowerGrid
  }

  def apply( dataModel: DataModel, hull: Hull ) : ShipModel = {
    val shipModules = hull.moduleSlotList.map( hullSlot => ShipModuleSlot(hullSlot.pos, "Dummy", 0.0f, None))
    val ship = Ship("New " + hull.name, hull.role, None, None, hull.race, hull.hullId, shipModules)
    apply( dataModel, hull, ship )
  }
}

class ShipModel( val hull: Hull, val ship: Ship, val combatState: String, val slots: Map[Point, ModelSlot]) {
  val (width, height) = if (slots.isEmpty) (0, 0)
                        else {
                          val width = slots.keys.maxBy(_.x).x
                          val height = slots.keys.maxBy(_.y).y
                          (width, height)
                        }
  val midPoint = width / 2

  val allSlots = slots.mapValues(_.hullSlot)
  val allModules = slots.mapValues(_.module).filter(_._2 != dummy)
  val allWeapons = slots.filter(_._2.module.weaponData.isDefined).mapValues{ slot =>
    (slot.facing, slot.module)
  }

  private def copy( hull: Hull = this.hull, ship: Ship = this.ship,
      combatState: String = this.combatState, slots: Map[Point, ModelSlot] = this.slots ) : ShipModel =
    return new ShipModel( hull, ship, combatState, slots )

  def meetsRestrictions( module: ShipModule, point: Point ) : Boolean = meetsRestrictions(module, slots(point))
  private def meetsRestrictions( module: ShipModule, slot: ModelSlot ) : Boolean = {
    val modSlot = slot.hullSlot

    module.restrictions match {
      case "I" => modSlot.restrictions.contains("I")
      case "O" => modSlot.restrictions.contains("O")
      case "IO" => modSlot.restrictions.contains("I") || modSlot.restrictions.contains("O")
      case "E" =>  modSlot.restrictions == "E"
      case "IOE" => true
    }
  }

  def slotsInRange( xRange: Range, yRange: Range ) = {
    val buffer = mutable.Map[Point, HullModuleSlot]()

    for {
      x <- xRange
      y <- yRange
      point = Point(x, y)
      if ( slots.contains( point ))
    } buffer.put(point, slots(point).hullSlot)

    buffer.toMap
  }


  private def modulesOverlapping( x: Range, y: Range ) = {
    slots.view.filter(_._2.module != dummy).filter{ tuple =>
      val (p, slot) = tuple

      ( p.x until p.x + slot.module.xSize overlapsWith x ) &&
      ( p.y until p.y + slot.module.ySize overlapsWith y )
    }.toMap
  }

  private def moduleOnPoint( p: Point ) = modulesOverlapping( p.x to p.x, p.y to p.y )

  def pointsCoveredByModule( p : Point ) = for {
    (point, slot) <- moduleOnPoint( p )
    pointX <- (point.x until point.x + slot.module.xSize)
    pointY <- (point.y until point.y + slot.module.ySize)
  } yield Point(pointX, pointY)

  def isValidPoint( point: Point ) = slots.contains(point)

  def removeModule( point: Point ) : ShipModel = {
    val moduleToRemove = moduleOnPoint(point).filter( _._2.module != dummy )

    if (moduleToRemove.isEmpty) return this

    val slotToUpdate = moduleToRemove.head

    val toBeReplaced = moduleToRemove.mapValues(_.copy(module = dummy, slotOption = None) )
    copy( slots = slots ++ toBeReplaced ).computePowerGrid
  }

  def moduleAt( point: Point ) = moduleOnPoint(point).values.map(_.module).filter(_ != dummy).headOption
  def weaponAt( point: Point ) = moduleOnPoint(point)
      .filter( _._2.module.weaponData.isDefined )
      .mapValues(_.module)
      .headOption

  def hasPower( point: Point ) = slots(point).power

  def placeModule( point: Point, newModule: ShipModule ) : ShipModel =
    placeModule( point, newModule, None )
  def placeModule( point: Point, newModule: ShipModule, option: Option[String] ) : ShipModel = {
    val xRange = point.x until point.x + newModule.xSize
    val yRange = point.y until point.y + newModule.ySize

    for {
      x <- xRange
      y <- yRange
      slot = slots.get(Point(x, y))
      canPlace = slot.isDefined && meetsRestrictions(newModule, slot.get)
    } if ( !canPlace ) return this

    val toBeRemoved = modulesOverlapping( xRange, yRange ).filter(_._2.module != dummy)
    val removed : Map[Point, ModelSlot] = toBeRemoved.map{ tuple =>
      val (p, slot) = tuple
      (p, slot.copy( module = dummy, slotOption = None))
    }

    val toBePlaced = removed + (point -> removed.get(point).getOrElse(slots(point)).copy( module = newModule, slotOption = option ) )
    val copyModel = copy( slots = slots ++ toBePlaced )

    if ( newModule.powerPlantData.isDefined ) copyModel.computePowerGrid
    else copyModel
  }

  def setFacing( p: Point, f: Float ) : ShipModel = {
    val slot = slots(p)
    copy( slots = slots + (p -> slot.copy( facing = f )) )
  }

  def withCombatState( cs: String ) = copy( combatState = cs )
  def withName( name: String ) = copy( ship = ship.copy( name = name ) )
  
  def calculateCost = allModules.values.map(_.cost).sum
  def calculatePowerCapacity = allModules.values
      .filter(_.powerPlantData.isDefined)
      .map(_.powerPlantData.get.powerStoreMax)
      .sum
  def calculateRecharge = {
    val modules = allModules.values
    val powerGeneration = modules.filter(_.powerPlantData.isDefined).map(_.powerPlantData.get.powerFlowMax).sum
    val powerUse = modules.map(_.powerDraw).sum
    powerGeneration - powerUse
  }
  def calculateRechargeAtWarp = "???"
  def calculateHitpoints = allModules.values.map(_.health).sum
  def calculateShieldPower = allModules.values
      .filter(_.shieldData.isDefined)
      .map(_.shieldData.get.shieldPower)
      .sum
  def calculateMass = allModules.values.map(_.mass).sum
  def calculateSublightSpeed = "???"
  def calculateFtlSpeed = "???"
  def calculateTurnRate = "???"
  def calculateOrdnanceCapacity = allModules.values
      .flatMap(_.ordnanceCapacity)
      .sum
  def calculateCargoSpace = allModules.values
      .flatMap(_.cargoCapacity)
      .sum
  def hasCommandModule = allModules.values
      .find(_.moduleType == "Command")
      .isDefined
  def hasEmptySlots = {
    val slots = mutable.Map[Point, HullModuleSlot](allSlots.toSeq:_*)

    allModules.foreach { tuple =>
      val (point, module) = tuple

      for {
        x <- point.x until point.x + module.xSize
        y <- point.y until point.y + module.ySize
      } slots -= Point(x, y)
    }

    !slots.isEmpty
  }
  
  private def computePowerGrid : ShipModel = {
    val dePowered = slots.mapValues(_.copy(power = false))

    val toBePowered = dePowered.filter(_._2.module != dummy).foldLeft(dePowered) { (map, tuple) =>
        val (modPoint, modSlot) = tuple
        addPoweredSlots(modPoint, modSlot.module, map, _.copy( power = true))
    }
    copy(slots = toBePowered)
  }

  private def addPoweredSlots( point: Point, module: ShipModule,
      changed: Map[Point, ModelSlot], f : ModelSlot => ModelSlot ) : Map[Point, ModelSlot] =
    if ( module.powerPlantData.isEmpty || module.powerPlantData.get.powerRadius == 0)
      changed
    else {
      changed ++ slotsPoweredBy(point, module).map{ point =>
        if ( changed.contains(point) ) (point, f(changed(point)) )
        else (point, f( slots(point) ) )
      }.toMap
    }

  private def slotsPoweredBy(point: Point, module: ShipModule) : Set[Point] = {
    if ( module.moduleType == "PowerConduit" ) return Set()
    val buffer = mutable.Map[Point, Int]()

    def addAdjacent( range: Int, p: Point) : Unit = {
      if (range == 0) return
      if (buffer.get(p).getOrElse(0) >= range) return

      val Point(x, y) = p
      if (slots.contains(p)) buffer.put(p, range)

      addAdjacent( range - 1, Point(x-1,y) )
      addAdjacent( range - 1, Point(x+1,y) )
      addAdjacent( range - 1, Point(x,y-1) )
      addAdjacent( range - 1, Point(x,y+1) )
    }

    for {
      x <- point.x until point.x + module.xSize
      y <- point.y until point.y + module.ySize
    } addAdjacent(module.powerPlantData.get.powerRadius + 1, Point(x, y))
    //We add one to the power radius to account for the block the module is on.

    val conduits = mutable.Set[Point]()

    def addConduits( p: Point ) {
      if (conduits.contains(p)) return;
      if ( !slots.contains(p)) return
      if ( slots(p).module.moduleType != "PowerConduit" ) return;

      conduits.add(p)

      val Point(x, y) = p
      addConduits( Point(x-1,y) )
      addConduits( Point(x+1,y) )
      addConduits( Point(x,y-1) )
      addConduits( Point(x,y+1) )
    }

    for {
      x <- (point.x - 1) until point.x + module.xSize + 1
      y <- point.y until point.y + module.ySize
    } addConduits(Point(x, y))

    for {
      x <- point.x until point.x + module.xSize
      y <- (point.y - 1) until point.y + module.ySize + 1
    } addConduits(Point(x, y))

    for {
      Point(x, y) <- conduits
    } addAdjacent( 5, Point(x, y))

    buffer.keySet
  }
}