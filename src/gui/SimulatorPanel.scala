package gui

import java.util.concurrent.Executors
import java.util.concurrent.Future
import javax.swing.SwingUtilities
import javax.swing.text.AbstractDocument
import javax.swing.text.AttributeSet
import javax.swing.text.DocumentFilter
import javax.swing.text.DocumentFilter.FilterBypass
import scala.swing.BoxPanel
import scala.swing.Component
import scala.swing.FlowPanel
import scala.swing.Label
import scala.swing.Orientation
import scala.swing.TextField
import scalaz.Scalaz.ToOptionIdOps
import scalaz.Scalaz.none
import org.jfree.chart.ChartPanel
import org.jfree.chart.JFreeChart
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.data.xy.XYSeries
import org.jfree.data.xy.XYSeriesCollection
import data.general.DataModel
import sim.ShipStatisticsOut
import sim.Simulator
import scala.swing.Alignment
import scala.swing.event.EditDone

class SimulatorPanel( data: DataModel ) extends BoxPanel(Orientation.Vertical) {

  private val executor = Executors.newSingleThreadExecutor

  private val energySeries = new XYSeries("")
  private val energyDomainAxis = new NumberAxis( "Time (s)" )
  private val energyRangeAxis = new NumberAxis( "Energy" )
  private val energyPlot = new XYPlot( new XYSeriesCollection(energySeries),
      energyDomainAxis, energyRangeAxis, new XYLineAndShapeRenderer(true, false) )
  private val energyChart = new JFreeChart("", energyPlot)
  private val energyChartPanel = new ChartPanel( energyChart )
  energyDomainAxis.setRange(0.0, 120.0)
  energyChart.removeLegend

  this.contents += Component.wrap( energyChartPanel )

  private val ordnanceSeries = new XYSeries("")
  private val ordnanceDomainAxis = new NumberAxis( "Time (s)" )
  private val ordnanceRangeAxis = new NumberAxis( "Ordnance" )
  private val ordnancePlot = new XYPlot( new XYSeriesCollection(ordnanceSeries),
      ordnanceDomainAxis, ordnanceRangeAxis, new XYLineAndShapeRenderer(true, false) )
  private val ordnanceChart = new JFreeChart("", ordnancePlot)
  private val ordnanceChartPanel = new ChartPanel( ordnanceChart )
  ordnanceDomainAxis.setRange(0.0, 120.0)
  ordnanceChart.removeLegend

  private var lastModel : Option[ShipModel] = none

  this.contents += Component.wrap( ordnanceChartPanel )

  private val simulationTime = new TextField {
    object IntegerFilter extends DocumentFilter {
      override def insertString(fb: FilterBypass, offs: Int, str: String, a: AttributeSet){
       if(str.forall((c)=>c.isDigit)) super.insertString(fb, offs, str, a)
      }
      override def replace(fb: FilterBypass, offs: Int, l: Int, str: String, a: AttributeSet){
       if(str.forall((c)=>c.isDigit)) super.replace(fb, offs, l, str, a)
      }
    }

    peer.getDocument().asInstanceOf[AbstractDocument].setDocumentFilter(IntegerFilter)
    text = "120"
    columns = 5
  }

  this.contents += new FlowPanel(FlowPanel.Alignment.Left)(
      new Label("Simulation Time: "), simulationTime, new Label("(s)") )
  listenTo(simulationTime)


  private var future: Option[Future[Any]] = none

  private def updateOnWt( model: ShipModel ) : Unit = Runnable {
    energySeries.clear
    ordnanceSeries.clear
    future.foreach(_.cancel(true))
    future = Runnable{
        Simulator.runSimulator(model, data, ShipStatsOut, simTime)
    }.runOnWt.some
  }.runOnEdt

  private def simTime: Int = simulationTime.text.toInt

  reactions += {
    case ShipModelChanged(model) => {
      lastModel = model.some
      updateOnWt(model)
    }
    case EditDone(field) if field == simulationTime => {
      val time = this.simTime
      energyDomainAxis.setRange(0.0, time)
      ordnanceDomainAxis.setRange(0.0, time)
      lastModel.foreach(updateOnWt)
    }
  }

  private object ShipStatsOut extends ShipStatisticsOut {
    private var frameCount = 0
    def emitDamage( damage: Double, empDamage: Double, direction: Double ) : Unit = Unit
    def frameComplete(currentEnergy: Double, currentOrdnance: Double, time: Double) : Unit = Runnable {
      frameCount += 1
      val redraw = frameCount % Simulator.FRAMES_PER_SECOND == 0
      energySeries.add(time, currentEnergy, redraw)
      ordnanceSeries.add(time, currentOrdnance, redraw)
    }.runOnEdt
  }

  private class Runnable( f: => Unit ) extends java.lang.Runnable {
    override def run() : Unit = f

    def runOnWt : Future[Any] = executor.submit(this).asInstanceOf[Future[Any]]
    def runOnEdt : Unit = SwingUtilities.invokeLater(this)
  }
  private object Runnable {
    def apply( f: => Unit ) : Runnable = new Runnable( f )
  }
}
