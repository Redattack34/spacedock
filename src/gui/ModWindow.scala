package gui

import scala.swing.Alignment
import scala.swing.BorderPanel
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.FlowPanel
import scala.swing.Frame
import scala.swing.Label
import scala.swing.Orientation
import scala.swing.ScrollPane
import scala.swing.Swing.VStrut
import data.general.DataModel
import data.xml.Mod
import javax.swing.BorderFactory
import javax.swing.border.BevelBorder
import scala.swing.Component
import scala.swing.event.ButtonClicked
import scala.swing.event.Event
import java.awt.Dimension
import scala.swing.ScrollBar
import data.general.Config


case class LoadMod(mod : Mod) extends Event
case class UnloadMod(mod : Mod) extends Event
case object ClearMods extends Event

class ModWindow(data: DataModel) extends Frame {
  
  title = "Select Mods"
  resizable = false
  
  private var modPanels : Map[String, ModPanel] = Map()
  
  private case class LoadModClicked(mod : Mod) extends Event
  private case class UnloadModClicked(mod : Mod) extends Event
  
  case object ClearButton extends Button("Clear")

  private case class LoadButton(mod: Mod) extends Button("  Load  ") {
    private var isLoad = true
    
    listenTo(this)
    
    def becomeUnload = {
      this.text = "Unload"
      this.isLoad = false
    }
    
    def becomeLoad = {
      this.text = "  Load  "
      this.isLoad = true
    }
    
    reactions += {
      case ButtonClicked(_) => {
        if ( isLoad ) {
          publish(LoadModClicked(mod))
          becomeUnload
        }
        else {
          publish(UnloadModClicked(mod))
          becomeLoad
        }
      }
    }
  }
  
  
  private class ModPanel(mod: Mod) extends BorderPanel {
    
    val nameLabel = new Label(mod.name)
    nameLabel.horizontalAlignment = Alignment.Left
    val desc = new Label(mod.desc)
    val load = LoadButton(mod)
    
    val loadPanel = new FlowPanel
    loadPanel.contents += load
    
    this.border = BorderFactory.createBevelBorder(BevelBorder.RAISED)
    this.add(nameLabel, BorderPanel.Position.North)
    this.add(desc, BorderPanel.Position.Center)
    this.add(loadPanel, BorderPanel.Position.East)
  }

  private val panels = new BoxPanel(Orientation.Vertical)
  
  private def addMod(mod: Mod) = {
    val panel = new ModPanel(mod)
    listenTo(panel.load)
    if ( panels.contents.length >= 1 ) {
    	panels.contents += VStrut(3)
    }
    panels.contents += panel
    modPanels += (mod.name -> panel)
  }
  data.mods.foreach(addMod)
  Config.mods.foreach(m => modPanels(m).load.becomeUnload)
  
  private val scroll = new ScrollPane(panels)
  scroll.horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
  scroll.preferredSize = new Dimension(scroll.preferredSize.width + 20, math.min(500, scroll.preferredSize.height))
  scroll.verticalScrollBar.unitIncrement = 10
  
  private val clearPanel = new FlowPanel
  clearPanel.contents += ClearButton
  listenTo(ClearButton)
  
  private val modPanel = new BorderPanel {
	  add(scroll, BorderPanel.Position.Center)
	  add(clearPanel, BorderPanel.Position.South)
  }
  
  contents = modPanel
  pack
  
  reactions += {
    case OpenModWindow => {
      this.centerOnScreen
      this.open
    }
    case ButtonClicked(ClearButton) => {
      modPanels.values.foreach(_.load.becomeLoad)
      Config.clearMods
      publish(ClearMods)
    }
    case LoadModClicked(mod) => {
      Config.addMod(mod.name)
      publish(LoadMod(mod))
    }
    case UnloadModClicked(mod) => {
      Config.removeMod(mod.name)
      publish(UnloadMod(mod))
    }
  }
}