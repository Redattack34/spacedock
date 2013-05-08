package gui

import java.awt.event.MouseEvent
import java.awt.event.MouseListener

import javax.swing.JTree
import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.tree.DefaultTreeModel
import javax.swing.tree.TreePath
import javax.swing.tree.TreeSelectionModel

import scala.swing.BorderPanel
import scala.swing.Component
import scala.swing.ScrollPane
import scala.swing.event.Event

import data.general.DataModel
import data.general.ReloadFromModel
import data.xml.ShipModule

class ModuleList( model: DataModel ) extends BorderPanel with MouseListener {

  import model._

  private class ModuleNode(val mod: ShipModule) extends DefaultMutableTreeNode(token(mod.nameIndex))

  type Node = DefaultMutableTreeNode

  private val root = new Node()

  private val weapon = new Node("Weapon")
  private val power = new Node("Power")
  private val defense = new Node("Defense")
  private val special = new Node("Special")

  private var nodeMap = Map[ShipModule, ModuleNode]()

  root.add(weapon)
  root.add(power)
  root.add(defense)
  root.add(special)

  var weaponTypeNodes : Map[String, Node] = Map()

  var moduleTypeNodes : Map[String, Node] = Map()

  private def addCategory( modType: String, node: Node ) : Unit = modType match {
    case "PowerPlant" | "FuelCell" | "Engine" | "PowerConduit" => power.add(node)
    case "Armor" | "Shield" => defense.add(node)
    case "Bomb" => Unit
    case _ => special.add(node)
  }

  private def toNode(mod: ShipModule) = {
    val node = new ModuleNode(mod)
    nodeMap += mod -> node
    node
  }

  private def assign( mod: ShipModule ) : Unit = mod.moduleType match {
    case "Bomb" => weaponTypeNodes("Bomb").add(toNode(mod))
    case "MissileLauncher" | "MainGun" | "Turret" | "Drone" => assignAsWeapon(mod)
    case str: String  if str.startsWith("Dual") => assignAsWeapon(mod)
    case _ => assignAsModule(mod)
  }

  private def assignAsWeapon( mod: ShipModule ) : Unit = {
    val weaponType = mod.weaponData.get.weaponType
    val weapon = model.weapon(weaponType)
    weapon.foreach { weap =>
      weaponTypeNodes(weap.weaponType).add(toNode(mod))
    }
  }

  private def assignAsModule( mod: ShipModule ) : Unit =
    moduleTypeNodes(mod.moduleType).add(toNode(mod))

  def loadModules = {
    weaponTypeNodes = weaponTypes
      .+("Bomb")
      .map((str : String) => ((str, new Node(str))))
    .toMap
    weaponTypeNodes.values.foreach(weapon.add(_))

    moduleTypeNodes = shipModules
      .filter(_.weaponData.isEmpty)
      .map(_.moduleType)
      .toSet
      .map((str: String) => ((str, new Node(str))))
    .toMap
    moduleTypeNodes.foreach( (addCategory _).tupled)

    shipModules.sortBy( mod => token(mod.nameIndex) ).foreach(assign(_))
  }
  loadModules

  val tree = new JTree(root)
  tree.setRootVisible(false)
  tree.addMouseListener(this)
  tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION)

  override def mouseReleased(ev: MouseEvent) = {
    val selected = tree.getLastSelectedPathComponent
    selected match {
      case node: ModuleNode => publish( ModuleSelected(node.mod))
      case _ => Unit
    }
  }

  override def mouseClicked(ev: MouseEvent) = Unit
  override def mousePressed(ev: MouseEvent) = Unit
  override def mouseEntered(ev: MouseEvent) = Unit
  override def mouseExited(ev: MouseEvent) = Unit

  add(new ScrollPane(Component.wrap(tree)), BorderPanel.Position.Center)

  reactions += {
    case ModulePickedUp(mod) => {
      val node = nodeMap(mod)
      val path = new TreePath(node.getPath().asInstanceOf[Array[Object]])
      tree.expandPath(path)
      tree.setSelectionPath(path)
      publish(ModuleSelected(mod))
    }
    case ReloadFromModel => {
      weapon.removeAllChildren()
      power.removeAllChildren()
      defense.removeAllChildren()
      special.removeAllChildren()
      loadModules
      val model = tree.getModel.asInstanceOf[DefaultTreeModel]
      model.reload
      repaint
    }
  }
}

case class ModuleSelected( module: ShipModule ) extends Event