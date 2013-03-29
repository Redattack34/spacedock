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
import gui.MouseClickWrapper.click2wrapper
import scala.swing.event.MouseReleased
import scala.swing.event.MouseDragged
import javax.swing.JOptionPane
import data.xml.ShipModule
import data.xml.Ship
import data.xml.ShieldData
import data.xml.HullModuleSlot

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
  case class FacingMode(p: Point, mod: ShipModule) extends EditorMode {
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

  private def resize : Unit = {
    val newWidth = shipModel.width + 20
    val newHeight = shipModel.height + 20
    val minSize = new Dimension( (newWidth + 2) * zoom, (newHeight + 2) * zoom )
    this.minimumSize = minSize
    this.preferredSize = minSize
    this.peer.setSize(minSize)

    repaint
  }

  background = Color.WHITE

  listenTo(mouse.moves)
  listenTo(mouse.clicks)

  reactions += {
    case HullSelected( newHull ) => shipModel = ShipModel( dataModel, newHull )
    case ShipSelected( newShip, newHull ) => shipModel = ShipModel( dataModel, newHull, newShip )
    case ZoomSet( newZoom ) => this.zoom = newZoom
    case FiringArcsSet( newShow ) => this.showArcs = newShow
    case ModuleSelected(mod: ShipModule) => this.mode = PlacementMode(mod)
    case SaveShip => {
      val name = JOptionPane.showInputDialog(this.peer, "Name:",
          "Save Ship", JOptionPane.QUESTION_MESSAGE)
      if ( name != null ) {
        shipModel = shipModel.withName(name)
        val saved = dataModel.save(shipModel)
        publish(ShipSaved(saved))
      }
    }

    case MouseMoved(comp, loc, _) if comp == this => mouseOver = Point((loc.x / zoom) - 10, (loc.y / zoom) - 10)
    case MouseDragged(comp, loc, _) if comp == this => {
      if (  mode.isFacing ) {
        val FacingMode(p, mod) = mode
        if ( mod.moduleType == "Turret" ) {
          val baseX = ((p.x + mod.xSize.toFloat / 2) + 10) * zoom
          val baseY = ((p.y + mod.ySize.toFloat / 2) + 10) * zoom

          val diffX = loc.x - baseX
          val diffY = baseY - loc.y

          val angleRad = math.atan2(diffY, diffX)
          val angleDeg = math.toDegrees(angleRad)

          shipModel = shipModel.setFacing(p, angleDeg.toFloat)
        }
      }
    }
    case cl: MousePressed if cl.isRight => rightClick
    case cl: MousePressed if cl.isMiddle => middleClick
    case cl: MousePressed if cl.isLeft => leftClick
  }

  private def dropModule = this.mode = NormalMode

  private def middleClick : Unit = {
    val clickedOn = shipModel.moduleAt(mouseOver)
    clickedOn.foreach{ mod => mode = PlacementMode(mod); publish(ModulePickedUp(mod))}
  }

  private def rightClick : Unit = {
    if ( shipModel.isValidPoint(mouseOver)) shipModel = shipModel.removeModule(mouseOver)
    else this.mode = NormalMode
  }

  private def leftClick : Unit = {
    mode match {
      case PlacementMode(mod) => {
        if ( mod.hangarData.isDefined && !mod.hangarData.get.isSupplyBay &&
                !mod.hangarData.get.isTroopBay) {
          val selected = JOptionPane.showInputDialog(this.peer, "", "Select Fighter",
              JOptionPane.QUESTION_MESSAGE, null,
              dataModel.fighterDesigns.asInstanceOf[Array[Object]], null)
          if ( selected == null ) return
          else shipModel = shipModel.placeModule(mouseOver, mod, Some(selected.toString))
        }
        else shipModel = shipModel.placeModule(mouseOver, mod)
      }
      case _ => {
          val weaponAt = shipModel.weaponAt(mouseOver)
                  weaponAt match {
                  case Some( (point, mod) ) => mode = new FacingMode(point, mod)
                  case None => Unit
          }
      }
    }
  }

  def getRect(x: Int, y: Int, w: Int = 1, h: Int = 1) =
    new Rectangle((x + 10) * zoom + 2, (y + 10) * zoom + 2, (w * zoom) - 4, (h * zoom) - 4)

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
       val FacingMode(p, _) = mode
       val (facing, module) = shipModel.allWeapons(p)
       drawFiringArc( g2, p, facing, module )
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

/*     if ( midPoint != -1 ) {
       g2.setColor(Color.BLACK)
       g2.drawLine(midPoint * zoom, 0, midPoint * zoom, this.size.height)
     }*/
  }

  private def drawSlot(g2: Graphics2D, p: Point, slot: HullModuleSlot ): Unit = {
    val Point(x, y) = p
    val rect = getRect(x, y)

    val xRange = mouseOver.x until mouseOver.x + mode.module.map(_.xSize).getOrElse(1)
    val yRange = mouseOver.y until mouseOver.y + mode.module.map(_.ySize).getOrElse(1)
    val blockColor = if ( shipModel.slotsInRange(xRange, yRange).contains(p) ) mouseOverColor
                else if ( !mode.isPlacement ) Color.WHITE
                else if ( shipModel.meetsRestrictions(mode.module.get, p) ) canPlaceColor
                else canNotPlaceColor
    g2.setColor(blockColor)
    g2.fill(rect)

    val squareColor = if ( shipModel.hasPower(p) ) Color.YELLOW
                      else Color.BLACK
    g2.setColor(squareColor)
    g2.draw(rect)

    g2.setColor(Color.BLACK)
    g2.setFont( g2.getFont().deriveFont(7.0f + (2 * (zoom/16))))
    g2.drawString(slot.restrictions, rect.x + 2, rect.y + 8 + (2 * (zoom/16)))
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