package gui

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.Rectangle
import java.awt.RenderingHints
import scala.swing.Component
import scala.swing.Orientation
import scala.swing.Scrollable
import scala.swing.event.Event
import scala.swing.event.MouseMoved
import scala.swing.event.MousePressed
import data.general.DataModel
import data.general.Point
import gui.MouseEventWrappers._
import scala.swing.event.MouseReleased
import scala.swing.event.MouseDragged
import javax.swing.JOptionPane
import data.xml.ShipModule
import data.xml.Ship
import data.xml.ShieldData
import data.xml.HullModuleSlot
import data.general.ReloadFromModel
import data.xml.ShipModule
import scala.swing.event.MouseEvent
import scala.swing.event.KeyTyped
import scala.swing.event.KeyReleased
import scala.swing.event.Key

case class ModulePickedUp( mod: ShipModule ) extends Event
case class ShipModelChanged( model: ShipModel ) extends Event
case class ShipSaved( ship: Ship ) extends Event
class ShipEditor(dataModel: DataModel) extends Component with Scrollable {

  type AwtPoint = java.awt.Point;

  sealed trait EditorMode {
    def module: Option[ShipModule] = None
    def isNormal: Boolean = false
    def isPlacement: Boolean = false
    def isFacing: Boolean = false
  }
  case object NormalMode extends EditorMode {
    override val isNormal = true
  }
  case class PlacementMode(mod: ShipModule) extends EditorMode {
    override val isPlacement = true
    override def module = Some(mod)
  }
  case class FacingMode(mods: Set[(Point, ShipModule)]) extends EditorMode {
    override val isFacing = true
  }

  private val mouseOverColor = new Color(122, 230, 255)
  private val canNotPlaceColor = new Color(255, 125, 147)
  private val canPlaceColor = new Color(179, 255, 117)
  private val shieldColor = new Color(0, 200, 255)

  def preferredViewportSize: Dimension = this.preferredSize

  def tracksViewportHeight: Boolean = true
  def tracksViewportWidth: Boolean = true

  def blockIncrement(visibleRect: Rectangle, orientation: Orientation.Value, direction: Int): Int = zoom
  def unitIncrement(visibleRect: Rectangle, orientation: Orientation.Value, direction: Int): Int = zoom

  private var _zoom = 16;
  private def zoom = _zoom
  private def zoom_=(z: Boolean) = {
    z match {
      case true => _zoom = 32
      case false => _zoom = 16
    }
    resize
  }

  private var _arcs = false;
  private def showArcs = _arcs;
  private def showArcs_=(a: Boolean) {
    _arcs = a
    repaint
  }
  
  private var _mirror = false;
  private def mirror = _mirror;
  private def mirror_=(a: Boolean) {
    _mirror = a
    repaint
  }

  def blockIncrement() = zoom

  private var _shipModel = ShipModel.empty
  private def shipModel = _shipModel
  private def shipModel_=( newModel: ShipModel ) : Unit = {
    val oldModel = _shipModel
    if ( newModel == oldModel ) return
    _shipModel = newModel
    publish(ShipModelChanged(newModel))
    if ( oldModel.width !=  newModel.width ||
         oldModel.height != newModel.height ) resize
    else repaint
  }

  private var _mouseOver : Point = Point(0, 0)
  private def mouseOver = _mouseOver
  private def mouseOver_=( p: Point ) = {
    if ( _mouseOver != p ) {
      _mouseOver = p
      repaint
    }
  }

  private var _mode: EditorMode = NormalMode
  private def mode = _mode
  private def mode_=(newMode: EditorMode) = {
    if ( newMode != _mode ) {
      _mode = newMode
      repaint
    }
  }
  
  private var _showEmpty = 0
  private def showEmpty = _showEmpty
  private def showEmpty_=(newShow: Int) = {
    this._showEmpty = newShow
    repaint
  }
  
  private def resize : Unit = {
    val newWidth = shipModel.width + 20
    val newHeight = shipModel.height + 20
    val minSize = new Dimension( (newWidth) * zoom, (newHeight) * zoom )
    this.minimumSize = minSize
    this.preferredSize = minSize
    this.peer.setSize(minSize)

    repaint
  }

  focusable = true

  listenTo(mouse.moves)
  listenTo(mouse.clicks)
  listenTo(keys)

  def getMouseOver( loc: AwtPoint ) = 
    Point((loc.x / zoom) - 10, (loc.y / zoom) - 10)
  
  def centerMod( c: Int, size: Int ) : Double = ((c + size.toFloat / 2) + 10) * zoom 
    
  def toAwtPoint( p: Point, xSize: Int, ySize: Int) = 
    new AwtPoint( centerMod(p.x, xSize).toInt, centerMod(p.y, ySize).toInt )
    
  def getAngle( mod: AwtPoint, mouse: AwtPoint ) : Double = {
    val diffX = mouse.x - mod.x
    val diffY = mod.y - mouse.y

    val angleRad = math.atan2(diffY, diffX)
    math.toDegrees(angleRad)
  }
  
  def pointToMouse( mouse: AwtPoint, p: Point, mod: ShipModule) : Double = 
    getAngle( toAwtPoint( p, mod.xSize, mod.ySize), mouse )
    
  def pointAwayFromMouse( mouse: AwtPoint, p: Point, mod: ShipModule) : Double =
    (pointToMouse(mouse, p, mod) + 180.0d) % 360.0d
    
  def shareDirection : (AwtPoint, Point, ShipModule) => Double = {
    val FacingMode(mods) = mode
    val seq = mods.toSeq
    
    val minX = seq.map(_._1.x).min
    val minY = seq.map(_._1.y).min
    
    val maxX = seq.map(t => t._1.x + t._2.xSize).max
    val maxY = seq.map(t => t._1.y + t._2.ySize).max
    
    val sizeX = maxX - minX
    val sizeY = maxY - minY
    
    val baseX = centerMod( minX, sizeX )
    val baseY = centerMod( minY, sizeY )
    
    val base = new AwtPoint( baseX.toInt, baseY.toInt )
    (mouse, p, ship) => getAngle( base, mouse )
  }
  
  val keyToAngle : PartialFunction[Key.Value, Double] = {
          case Key.Numpad6 =>   0d
          case Key.Numpad9 =>  45d
          case Key.Numpad8 =>  90d
          case Key.Numpad7 => 135d
          case Key.Numpad4 => 180d
          case Key.Numpad1 => 225d
          case Key.Numpad2 => 270d
          case Key.Numpad3 => 315d
        }
  
  def constantAngle( angle: Double ) : (AwtPoint, Point, ShipModule) => Double = {
    (mouse, p, ship) => angle
  }
    
  def changeFacing( mouse: AwtPoint )( ang: (AwtPoint, Point, ShipModule) => Double)
      ( model: ShipModel, pair: (Point, ShipModule)) : ShipModel = {
    val (p, mod) = pair
    mirrorChange( model, mod.xSize, p, ang(mouse, p, mod),
      (ship, p, f) => ship.setFacing(p, f.toFloat) )
  }
    
  reactions += {
    case ReloadFromModel => {
      shipModel = shipModel.reload(dataModel)
      mode = NormalMode
    }
    case HullSelected( newHull ) => shipModel = ShipModel( dataModel, newHull )
    case ShipSelected( newShip, newHull ) => shipModel = ShipModel( dataModel, newHull, newShip )
    case ZoomSet( newZoom ) => this.zoom = newZoom
    case FiringArcsSet( newShow ) => this.showArcs = newShow
    case MirroringSet( newMirror ) => this.mirror = newMirror
    case ShowEmptySlots( ) => this.showEmpty = 100
    case ModuleSelected(mod) => this.mode = PlacementMode(mod)
    case CombatStateSet(state) => this.shipModel = shipModel.withCombatState(state)
    case SaveShip => {
      val name = JOptionPane.showInputDialog(this.peer, "Name:",
          "Save Ship", JOptionPane.QUESTION_MESSAGE)
      if ( name != null ) {
        shipModel = shipModel.withName(name)
        val saved = dataModel.save(shipModel)
        publish(ShipSaved(saved))
      }
    }

    case MouseMoved(comp, loc, _) if comp == this => mouseOver = getMouseOver(loc)
    case e@MouseDragged(comp, loc, _) if comp == this => {
      this.requestFocus
      val oldMouseOver = mouseOver
      mouseOver = getMouseOver(loc)
      if (  mode.isFacing ) {
        val FacingMode(mods) = mode
        val filtered = mods.filter(_._2.moduleType == "Turret")
        val angleFunc =
          if ( e.isAltDown ) pointAwayFromMouse _
          else if ( e.isCtrlDown ) pointToMouse _
          else shareDirection
        shipModel = filtered.foldLeft(shipModel)(changeFacing(loc)(angleFunc) _)
      }
      if ( mode.isPlacement && oldMouseOver != mouseOver ) {
        leftClick(e)
      }
    }
    case cl: MousePressed if cl.isRight => { this.requestFocus; rightClick }
    case cl: MousePressed if cl.isMiddle => { this.requestFocus; middleClick }
    case cl: MousePressed if cl.isLeft => { this.requestFocus; leftClick(cl) }
    
    case keyReleased : KeyReleased => {
      if ( mode.isFacing && keyToAngle.isDefinedAt(keyReleased.key)) {
        val FacingMode(mods) = mode
        
        val angle = keyToAngle(keyReleased.key)
        
        val filtered = mods.filter(_._2.moduleType == "Turret")
        shipModel = filtered.foldLeft(shipModel)(changeFacing(null)(constantAngle(angle)) _)
      }
    }
  }
  
  private def dropModule = this.mode = NormalMode
  
  def mirrorChange(model: ShipModel, xSize: Int, p: Point,
      f: (ShipModel, Point) => ShipModel ) : ShipModel = 
    mirrorChange(model, xSize, p, 0.0d, (ship, p, ang) => f(ship, p))
  def mirrorChange(model: ShipModel, xSize: Int, p: Point, angleDeg: Double,
      f : (ShipModel, Point, Double) => ShipModel ) : ShipModel = {
    val newModel = f( model, p, angleDeg )
    if ( this.mirror && 
       ( (p.x          >= model.midPoint + 1) || 
         (p.x + xSize  <= model.midPoint + 1) ) ) {
      f( newModel, reflected( p, xSize ),  180.0d - angleDeg )
    }
    else newModel
  }

  private def middleClick : Unit = {
    val clickedOn = shipModel.moduleAt(mouseOver)
    clickedOn.foreach{ mod => mode = PlacementMode(mod); publish(ModulePickedUp(mod))}
  }

  private def rightClick : Unit = {
    if ( shipModel.isValidPoint(mouseOver)) {
      shipModel = mirrorChange(shipModel, 1, mouseOver, _.removeModule(_) )
    }
    else this.mode = NormalMode
  }
  
  private def leftClick(e: MouseEvent) : Unit = {
    mode match {
      case PlacementMode(mod) => {
        if ( mod.hangarData.isDefined && !mod.hangarData.get.isSupplyBay &&
                !mod.hangarData.get.isTroopBay) {
          val selected = JOptionPane.showInputDialog(this.peer, "", "Select Fighter",
              JOptionPane.QUESTION_MESSAGE, null,
              dataModel.fighterDesigns.asInstanceOf[Array[Object]], null)
          if ( selected == null ) return
          else {
            shipModel = mirrorChange( shipModel, mod.xSize, mouseOver, _.placeModule(_, mod, Some(selected.toString)))
          }
        }
        else {
          shipModel = mirrorChange( shipModel, mod.xSize, mouseOver, _.placeModule(_, mod) )
        }
      }
      case _ => {
        val weaponAt = shipModel.weaponAt(mouseOver)
        weaponAt.foreach{ tuple =>
          val (point, mod) = tuple
          val mods : Set[(Point, ShipModule)] = mode match {
            case FacingMode(mods) => mods
            case _ => Set()
          }
          
          val newMods = if ( e.isShiftDown ) {
            if ( mods.contains(tuple) ) mods - tuple
            else mods + tuple
          } else Set(tuple)
          mode = FacingMode(newMods)
        }
      }
    }
  }

  def getRect(x: Int, y: Int, w: Int = 1, h: Int = 1) =
    new Rectangle((x + 10) * zoom + 2, (y + 10) * zoom + 2, (w * zoom) - 4, (h * zoom) - 4)

  
  def reflected(p: Point, xSize: Int ) : Point = {
    val mid = shipModel.midPoint
    if ( p.x < mid ) Point( mid + (mid - p.x) + 1 - (xSize - 1), p.y)
    else             Point( mid - (p.x - mid) + 1 - (xSize - 1), p.y)
  }
  
  override def paint( g2: Graphics2D ) : Unit = {
    super.paint(g2)

    for {
      (p, slot) <- shipModel.allSlots
    } drawSlot(g2, p, slot)

    for {
      (p, module) <- shipModel.allModules
    } drawModule( g2, p, module )

     for {
      (p, module) <- shipModel.allModules.view
      if ( module.powerDraw > 0)
      if ( !shipModel.pointsCoveredByModule(p).exists(shipModel.hasPower) )
    } drawLightningBolt( g2, p, module )

    if ( showArcs ) {
      for {
        (p, (facing, module)) <- shipModel.allWeapons.view
      } drawFiringArc( g2, p, facing, module )
    }
    else if ( mode.isFacing ) {
      val FacingMode(mods) = mode
      for {
        (p, _) <- mods
        (facing, module) = shipModel.allWeapons(p)
      } {
        drawFiringArc( g2, p, facing, module )
        if ( mirror ) {
          val p2 = reflected(p, module.xSize)
          shipModel.allWeapons.get(p2).foreach{ tuple =>
            val (facing2, module2) = tuple
            drawFiringArc( g2, p2, facing2, module2 )
          }
        }
      }
    }
    
    if (showArcs) {
      for {
        (p, module) <- shipModel.allModules
        shield <- module.shieldData
      } drawShieldRadius( g2, p, module, shield )
    } 
    if (mode.isPlacement) {
      val PlacementMode(module) = mode
      module.shieldData.foreach(s => drawShieldRadius(g2, mouseOver, module, s))
    }

    if ( mirror ) {
      val midpointLine = shipModel.midPoint + 11
      if ( midpointLine != -1 ) {
        g2.setColor(Color.BLACK)
        g2.setStroke(new BasicStroke(1))
        g2.drawLine(midpointLine * zoom, 0, midpointLine * zoom, this.size.height)
      }
    }
    
    if ( showEmpty > 0) { showEmpty -= 1 }
  }

  private def drawSlot(g2: Graphics2D, p: Point, slot: HullModuleSlot ): Unit = {
    val Point(x, y) = p
    val rect = getRect(x, y)
    
    val xSize = mode.module.map(_.xSize).getOrElse(1)
    val ySize = mode.module.map(_.ySize).getOrElse(1)

    val xRange = mouseOver.x until mouseOver.x + xSize
    val yRange = mouseOver.y until mouseOver.y + ySize
    val mirror = if ( this.mirror && 
                    ( (mouseOver.x          >= shipModel.midPoint + 1) || 
                      (mouseOver.x + xSize  <= shipModel.midPoint + 1) ) ) reflected(mouseOver, xSize )
                 else mouseOver
    val xRangeMirror = mirror.x until mirror.x + xSize
    val yRangeMirror = mirror.y until mirror.y + ySize
    val blockColor = if ( xRange.contains(p.x) && yRange.contains(p.y) ) mouseOverColor
                else if ( xRangeMirror.contains(p.x) && yRangeMirror.contains(p.y)) mouseOverColor 
                else if ( !mode.isPlacement ) Color.WHITE
                else if ( shipModel.meetsRestrictions(mode.module.get, p) ) canPlaceColor
                else canNotPlaceColor
    g2.setColor(blockColor)
    g2.fill(rect)

    val squareColor = if ( showEmpty > 0 && shipModel.moduleAt(p).isEmpty ) {
                        if ( (showEmpty/5) % 2 == 0 ) Color.RED
                        else Color.BLACK
                      }
                      else if ( shipModel.hasPower(p) ) Color.YELLOW
                      else Color.BLACK
    g2.setColor(squareColor)
    g2.draw(rect)

    g2.setColor(Color.BLACK)
    g2.setFont( g2.getFont().deriveFont(7.0f + (2 * (zoom/16))))
    g2.drawString(slot.restrictions.str, rect.x + 2, rect.y + 8 + (2 * (zoom/16)))
  }

  private def drawModule( g2: Graphics2D, p: Point, mod: ShipModule ) : Unit = {
    val Point(x, y) = p
    val rect = getRect(x, y, mod.xSize, mod.ySize)

    g2.drawImage(dataModel.moduleImage(mod).getImage,
        rect.x, rect.y, rect.width, rect.height, null)
  }

  private def drawLightningBolt( g2: Graphics2D, p: Point, mod: ShipModule ) {
    val Point(x, y) = p
    val rect = getRect(x, y, mod.xSize, mod.ySize)

    g2.drawImage(dataModel.lightningBolt.getImage,
        rect.x, rect.y, rect.width, rect.height, null)
  }

  private def drawFiringArc( g2: Graphics2D, p: Point, facing: Float, mod: ShipModule ) {
    val baseX = ((p.x + mod.xSize.toFloat / 2) + 10) * zoom
    val baseY = ((p.y + mod.ySize.toFloat / 2) + 10) * zoom

    val r : Double = 100 * (zoom / 16)
    val spread = math.toRadians( mod.weaponData.get.fieldOfFire.toDouble / 2 )
    val facingRad = math.toRadians(facing)

    val arc1X = r * math.cos(facingRad + spread)
    val arc1Y = r * -math.sin(facingRad + spread)

    val arc2X = r * math.cos(facingRad - spread)
    val arc2Y = r * -math.sin(facingRad - spread)

    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2.setColor(Color.YELLOW)
    g2.setStroke(new BasicStroke(3))
    g2.drawLine(baseX.toInt, baseY.toInt, (baseX + arc1X).toInt, (baseY + arc1Y).toInt);
    g2.drawLine(baseX.toInt, baseY.toInt, (baseX + arc2X).toInt, (baseY + arc2Y).toInt);
    g2.drawArc((baseX - r).toInt, (baseY - r).toInt, (2 * r).toInt, (2 * r).toInt,
        math.toDegrees(facingRad - spread).toInt, math.toDegrees(2 * spread).toInt)
  }
  
  private def drawShieldRadius( g2: Graphics2D, p: Point, mod: ShipModule, shield: ShieldData ) {
    val radius = (shield.radius.toDouble / 16) * zoom
    val diameter = (radius * 2).toInt
    
    val baseX = ((p.x + mod.xSize.toFloat / 2) + 10) * zoom
    val baseY = ((p.y + mod.ySize.toFloat / 2) + 10) * zoom
    
    val x = baseX - radius
    val y = baseY - radius
    
    g2.setColor(shieldColor)
    g2.drawOval(x.toInt, y.toInt, diameter, diameter)
  }
}