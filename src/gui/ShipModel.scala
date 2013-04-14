package gui

import data.xml.Hull._
import data.xml.Module._
import data.xml.Ship._
import data.general.DataModel
import data.xml.Module._
import data.general.Point
import data.general.RangeOverlap._
import scala.collection._
import data.xml.Hull
import data.xml.ShipModule
import data.xml.Ship
import data.xml.HullModuleSlot
import data.xml.ShipModuleSlot
import data.xml.ShipModule
import scalaz.Scalaz._

case class ModelSlot( hullSlot: HullModuleSlot, module: ShipModule, power: Boolean, facing: Float, slotOption: Option[String] )

object ShipModel {

  private val emptyHull : Hull = Hull("", "", "", "", "", None, Seq(), "", "", Seq() )
  private val emptyShip : Ship = Ship("", "", None, None, "", "", Seq(), Seq() )

  val empty = apply(null, emptyHull, emptyShip)

  def apply( dataModel : DataModel, hull: Hull, ship : Ship ) : ShipModel = {

    val hullSlots = hull.moduleSlotList.map( slot => (slot.pos, slot)).toMap
    val shipModules = ship.moduleSlotList.map( slot => (slot.pos, slot)).toMap
    val modelSlots = hullSlots.mapValues{ slot =>
      val shipSlot = shipModules.get(slot.pos)
      val module = shipSlot.map(_.installed) match {
        case Some(x) if x =/= "Dummy" => dataModel.module(x)
        case _ => dummy
      }

      ModelSlot( slot, module, false, shipSlot.map(_.facing).getOrElse(90.0f),
          shipSlot.flatMap(_.slotOptions) )
    }

    val pointSlots = modelSlots.map{ case (point, slot) => ( Point(point.x / 16, point.y / 16), slot) }

    val minSlots = if ( pointSlots.isEmpty ) pointSlots
                   else {
                     val minX = pointSlots.keys.minBy(_.x).x
                     val minY = pointSlots.keys.minBy(_.y).y

                     pointSlots.map{ case (point, slot) => ( Point(point.x - minX, point.y - minY), slot ) }
                   }

    new ShipModel( hull, ship, ship.combatState.getOrElse(AttackRuns), minSlots ).computePowerGrid
  }

  def apply( dataModel: DataModel, hull: Hull ) : ShipModel = {
    val shipModules = hull.moduleSlotList.map( hullSlot => ShipModuleSlot(hullSlot.pos, "Dummy", 90.0f, None))
    val ship = Ship("New " + hull.name, hull.role, None, None, hull.race, hull.hullId, shipModules, dataModel.loadedMods)
    apply( dataModel, hull, ship )
  }
}

class ShipModel( val hull: Hull, val ship: Ship, val combatState: CombatState, val slots: Map[Point, ModelSlot]) {
  val (width, height) = if (slots.isEmpty) (0, 0)
                        else {
                          val width = slots.keys.maxBy(_.x).x
                          val height = slots.keys.maxBy(_.y).y
                          (width, height)
                        }
  val midPoint = (width.toDouble + 1) / 2

  val allSlots = slots.mapValues(_.hullSlot)
  val allModules = slots.mapValues(_.module).filter{notDummy}
  val allWeapons = slots.filter(_._2.module.weaponData.isDefined).mapValues{ slot =>
    (slot.facing, slot.module)
  }
  
  private def notDummy( t: Tuple2[_,_] ) : Boolean = t match {
    case (_, mod: ShipModule) => notDummy(mod)
    case (_, slot: ModelSlot) => notDummy(slot.module)
  }
  private def notDummy( mod: ShipModule ) : Boolean = mod != dummy

  private def copy( hull: Hull = this.hull, ship: Ship = this.ship,
      combatState: CombatState = this.combatState, slots: Map[Point, ModelSlot] = this.slots ) : ShipModel =
    return new ShipModel( hull, ship, combatState, slots )

  def meetsRestrictions( module: ShipModule, point: Point ) : Boolean = meetsRestrictions(module, slots(point))
  private def meetsRestrictions( module: ShipModule, slot: ModelSlot ) : Boolean =
    module.restrictions.matches( slot.hullSlot.restrictions )

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


  def modulesOverlapping( x: Range, y: Range ) = {
    slots.view.filter(notDummy).filter{ case (p, slot) =>
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
    val moduleToRemove = moduleOnPoint(point).filter(notDummy)

    if (moduleToRemove.isEmpty) return this

    val slotToUpdate = moduleToRemove.head

    val toBeReplaced = moduleToRemove.mapValues(_.copy(module = dummy, slotOption = None, facing = 90.0f) )
    copy( slots = slots ++ toBeReplaced ).computePowerGrid
  }

  def moduleAt( point: Point ) = moduleOnPoint(point).values.map(_.module).filter(notDummy).headOption
  def weaponAt( point: Point ) = moduleOnPoint(point)
      .filter( _._2.module.weaponData.isDefined )
      .mapValues(_.module)
      .headOption

  def hasPower( point: Point ) = slots(point).power

  def canPlace( point: Point, module: ShipModule ) : Boolean = {
    val xRange = point.x until point.x + module.xSize
    val yRange = point.y until point.y + module.ySize

    for {
      x <- xRange
      y <- yRange
      slot = slots.get(Point(x, y))
      canPlace = slot.isDefined && meetsRestrictions(module, slot.get)
    } if ( !canPlace ) return false
    return true
  }


  def placeModule( point: Point, newModule: ShipModule ) : ShipModel =
    placeModule( point, newModule, None )
  def placeModule( point: Point, module: ShipModule, option: Option[String] ) : ShipModel = {
    val model = placeWithoutComputingPowerGrid(point, module, option)
    if ( module.powerPlantData.isDefined ) model.computePowerGrid
    else model
  }

  private def placeWithoutComputingPowerGrid( point: Point, module: ShipModule, option: Option[String] ) : ShipModel = {
      if ( !canPlace(point, module) ) return this;

      val xRange = point.x until point.x + module.xSize
      val yRange = point.y until point.y + module.ySize

      val toBeRemoved = modulesOverlapping( xRange, yRange ).filter(notDummy)
      val removed : Map[Point, ModelSlot] = toBeRemoved.map{ case (p, slot) =>
        (p, slot.copy( module = dummy, slotOption = None, facing = 90.0f))
      }

      val toBePlaced = removed + (point -> removed.get(point).getOrElse(slots(point)).copy( module = module, slotOption = option ) )
              copy( slots = slots ++ toBePlaced )
  }

  def setFacing( p: Point, f: Float ) : ShipModel = {
    val slot = slots(p)
    if ( slot.module.weaponData.isDefined )
      copy( slots = slots + (p -> slot.copy( facing = f )) )
    else
      this
  }

  def withCombatState( cs: CombatState ) = copy( combatState = cs )
  def withName( name: String ) = copy( ship = ship.copy( name = name ) )

  def reload( data: DataModel ) : ShipModel = {
    if ( !data.races.contains(hull.race)) return ShipModel.empty
    if ( !data.hulls(hull.race).contains(hull) ) return ShipModel.empty
    val allModules = data.modules
    val newModel = slots.filter(notDummy)
       .filter( t => allModules.contains( t._2.module.uid ) )
       .foldLeft(ShipModel(data, hull))( (acc, slot) =>
         acc.placeModule(slot._1, data.module(slot._2.module.uid))
       )
    newModel.copy( ship = ship.copy( requiredModsList = data.loadedMods))
  }

  def fillEmptySlots(mod: ShipModule) : ShipModel = {
    val leftRange = (0 to midPoint.toInt + 1)
    val rightRange = (width to midPoint.toInt by -1)
    val xRange = leftRange.toSeq ++ rightRange

    val model = (0 to height).foldLeft(this){ (model, y) =>
      xRange.foldLeft(model){ (model, x) =>
        val point = Point(x, y)
        if ( model.canPlace(Point(x, y), mod) &&
             model.modulesOverlapping(x until x + mod.xSize, y until y + mod.ySize ).isEmpty
           ) model.placeWithoutComputingPowerGrid(Point(x, y), mod, None)
        else model
      }
    }
    if ( mod.powerPlantData.isDefined ) model.computePowerGrid else model
  }

  val cost = allModules.values.map(_.cost).sum
  val upkeep = allSlots.size.toDouble * 0.01
  val powerCapacity = allModules.values
      .flatMap(_.powerPlantData)
      .map(_.powerStoreMax)
      .sum
  val totalPowerGeneration = allModules.values.flatMap(_.powerPlantData).map(_.powerFlowMax).sum
  val powerUse = allModules.values.map(_.powerDraw).sum
  val recharge = totalPowerGeneration - powerUse
  val rechargeAtWarp = totalPowerGeneration - (2 * powerUse)
  val hitpoints = allModules.values.map(_.health).sum
  val shieldPower = allModules.values
      .flatMap(_.shieldData)
      .map(_.shieldPower)
      .sum
  val mass = {
    val hullMass = (allSlots.size / 2)
    val moduleMass = math.max( 0, allModules.values.map(_.mass).sum )
    hullMass + moduleMass
  }
  val engines = allModules.values.flatMap(_.engineData)
  val sublightThrust = engines.map(_.thrust).sum
  val warpThrust = engines.map(_.warpThrust).sum
  val turnThrust = engines.map(_.turnThrust).sum
  val sublightSpeed = if ( mass === 0 ) 0 else sublightThrust / mass
  val ftlSpeed = if ( mass === 0 ) 0 else ( warpThrust / mass ) * 35
  val turnRate = if ( mass === 0 ) 0 else math.toDegrees(( turnThrust.toDouble / mass ) / 700)
  val ordnanceCapacity = allModules.values
      .flatMap(_.ordnanceCapacity)
      .sum
  val cargoSpace = allModules.values
      .flatMap(_.cargoCapacity)
      .sum
  val hasCommandModule = allModules.values
      .exists(mod => mod.uid === "CIC" || mod.uid ==="Cockpit" || mod.uid === "Bridge" )

  val hasEmptySlots = allSlots.keys.exists( moduleAt(_).isEmpty )

  private def computePowerGrid : ShipModel = {
    val dePowered = slots.mapValues(_.copy(power = false))

    val toBePowered = dePowered.filter(notDummy).foldLeft(dePowered) { case (map, (modPoint, modSlot)) =>
        addPoweredSlots(modPoint, modSlot.module, map, _.copy( power = true))
    }
    copy(slots = toBePowered)
  }

  private def addPoweredSlots( point: Point, module: ShipModule,
      changed: Map[Point, ModelSlot], f : ModelSlot => ModelSlot ) : Map[Point, ModelSlot] =
    if ( module.powerPlantData.isEmpty || module.powerPlantData.get.powerRadius === 0)
      changed
    else {
      changed ++ slotsPoweredBy(point, module).map{ point =>
        if ( changed.contains(point) ) (point, f(changed(point)) )
        else (point, f( slots(point) ) )
      }.toMap
    }

  private def slotsPoweredBy(point: Point, module: ShipModule) : Set[Point] = {
    if ( module.moduleType === "PowerConduit" ) return Set()
    val buffer = mutable.Map[Point, Int]()

    def addAdjacent( range: Int, p: Point) : Unit = {
      if (range === 0) return
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
      if ( slots(p).module.moduleType =/= "PowerConduit" ) return;

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