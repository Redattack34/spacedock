package gui

import scala.swing.BorderPanel
import scala.swing.Component
import scala.swing.Panel
import scala.swing.Publisher
import scala.swing.ScrollPane
import scala.swing.event.Event
import data.general.DataModel
import data.xml.Module.ShipModule
import javax.swing.JTree
import javax.swing.event.TreeSelectionEvent
import javax.swing.event.TreeSelectionListener
import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.tree.TreeNode
import javax.swing.tree.TreeSelectionModel
import javax.swing.tree.TreePath

class ModuleList( model: DataModel ) extends BorderPanel with TreeSelectionListener {

  import model._

  private class ModuleNode(val mod: ShipModule) extends DefaultMutableTreeNode(tokens(mod.nameIndex)) {

  }

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

  val weaponTypeNodes =
    weapons.values
      .map(_.weaponType).toSet.+("Bomb")
      .map((str : String) => ((str, new Node(str))))
    .toMap
  weaponTypeNodes.values.foreach(weapon.add(_))

  val moduleTypeNodes =
    modules.values
      .filter(_.weaponData.isEmpty)
      .map(_.moduleType)
      .toSet
      .map((str: String) => ((str, new Node(str))))
    .toMap
  moduleTypeNodes.foreach( (addCategory _).tupled)

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
    val trimmedWeaponType = weaponType.replaceFirst("Dual", "")
    val weapon = weapons(trimmedWeaponType)
    weaponTypeNodes(weapon.weaponType).add(toNode(mod))
  }

  private def assignAsModule( mod: ShipModule ) : Unit =
    moduleTypeNodes(mod.moduleType).add(toNode(mod))

  modules.values.toSeq.sortBy( mod => tokens(mod.nameIndex) ).foreach(assign(_))

  val tree = new JTree(root)
  tree.setRootVisible(false)
  tree.addTreeSelectionListener(this)
  tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION)

  override def valueChanged( event: TreeSelectionEvent ) = {
    val selected = tree.getLastSelectedPathComponent
    selected match {
      case node: ModuleNode => publish( ModuleSelected(node.mod))
      case _ => Unit
    }
  }

  add(new ScrollPane(Component.wrap(tree)), BorderPanel.Position.Center)

  reactions += {
    case ModulePickedUp(mod) => {
      val node = nodeMap(mod)
      val path = new TreePath(node.getPath().asInstanceOf[Array[Object]])
      tree.expandPath(path)
      tree.setSelectionPath(path)
    }
  }
}

case class ModuleSelected( module: ShipModule ) extends Event