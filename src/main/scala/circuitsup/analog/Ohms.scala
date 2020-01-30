package circuitsup.analog

import circuitsup.analog.Ohms.page1.nextButton
import circuitsup.{CircuitsRoute, Common, Router}
import circuitsup.templates.{ExerciseStage, ScatterPlot}
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, ^}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Open}
import com.wbillingsley.wren.{Circuit, Constraint, ConstraintPropagator, CurrentSource, Ground, Orientation, Resistor, Value, ValueLabel, ValueSlider, VoltageSource, Wire}
import org.scalajs.dom.{Element, Node}

import scala.collection.mutable

object Ohms {

  import Analog.{onCompletionUpdate, nextButton}

  class ScatterPlotData(x:Value, y:Value) {

    val data = mutable.Map.empty[Double, Double]

    def update():Unit = {
      for {
        (xx, _) <- x.value
        (yy, _) <- y.value
      } data(xx) = yy
    }

    def dataValues():Seq[(Double, Double)] = data.toSeq
  }

  object page1 extends ExerciseStage {

    var completion: Challenge.Completion = Open

    def onUpdate():Unit = {
      propagator.clearCalculations()
      propagator.resolve()

      plotData.update()
      if (plotData.dataValues().length > 50) {
        completion = Challenge.Complete(Some(1), None)
        onCompletionUpdate()
      } else {
        rerender()
      }
   }

    val vs = new VoltageSource((100, 150), orientation = Orientation.North, initial = Some(5))
    val r1 = new Resistor((300,150), orientation = Orientation.South)
    val gnd = Ground(100 -> 200)

    val w1 = new Wire(vs.t2, r1.t1, 100 -> 100, 300 -> 100)
    val w2 = new Wire(vs.t1, gnd.terminal)
    val w3 = new Wire(gnd.terminal, r1.t2, 300-> 200)

    val vl = new ValueLabel("V" -> "cc", vs.voltage, (130, 150), "left", Seq(ValueLabel.voltageMarkers((130, 120), (130, 180))))
    val rl = new ValueLabel("R" -> "1", r1.resistance, (320, 150), "left")
    val il = new ValueLabel("I" -> "1", r1.t1.current, (130, 80), "left", Seq(ValueLabel.currentArrow((150, 90), Orientation.East)))
    val rs = new ValueSlider(r1.resistance, (320, 160), min="100", max="10000", step="100")(onUpdate=onUpdate)

    val circuit = Circuit(Seq(gnd, vs, vl, r1, rl, il, w1, w2, w3, rs), 600, 300)
    val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))

    val plotData = new ScatterPlotData(r1.resistance, r1.t1.current)

    override protected def render: DiffNode[Element, Node] = <.div(Challenge.textAndEx(
      <.div(
        Common.marked(
          """
            |### Ohm's Law
            |
            |Resistance is the relationship between the voltage across two nodes and the current that flows between them.
            |It's defined as `V = IR`.
            |
            |On the right hand side, we've got a voltage source supplying 5V and a resistor. Vary the resistance and
            |see how it charts the current that flows through the resistor.
            |
            |The task will complete when there's at least 50 data points on the chart.
            |
            |""".stripMargin),
        completion match {
          case Complete(_, _) =>
            <.div(
              Common.marked("`I = V / R`, so the current drops off quickly as the resistance rises."),
              nextButton()
            )
          case _ => <.div()
        }
      )
    )(
      <.div(
        circuit,
        ScatterPlot(600, 300, "Resistance", "Current", (d) => r1.resistance.stringify(d), (d) => r1.t1.current.stringify(d), 10000, 0.05).plot(plotData.dataValues())
      )
    ))
  }

  object page3 extends ExerciseStage {

    var completion: Challenge.Completion = Open

    def onUpdate():Unit = {
      propagator.clearCalculations()
      propagator.resolve()

      plotData.update()
      if (plotData.dataValues().length > 50) {
        completion = Challenge.Complete(Some(1), None)
        onCompletionUpdate()
      } else {
        rerender()
      }
    }

    val vs = new CurrentSource((100, 150), orientation = Orientation.North, initial = Some(0.01))
    val r1 = new Resistor((300,150), orientation = Orientation.South)
    val gnd = Ground(100 -> 200)

    val w1 = new Wire(vs.t2, r1.t1, 100 -> 100, 300 -> 100)
    val w2 = new Wire(vs.t1, gnd.terminal)
    val w3 = new Wire(gnd.terminal, r1.t2, 300-> 200)

    val il = new ValueLabel("I" -> " ", vs.current, (130, 150))
    val rl = new ValueLabel("R" -> "1", r1.resistance, (320, 150), "left")
    val vl = new ValueLabel("V" -> "1", r1.voltage, (280, 150), "right", Seq(ValueLabel.voltageMarkers((280, 130), (280, 170))))
    val rs = new ValueSlider(r1.resistance, (320, 160), min="100", max="1000", step="10")(onUpdate=onUpdate)

    val circuit = Circuit(Seq(gnd, vs, vl, r1, rl, il, w1, w2, w3, rs), 600, 400)
    val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))

    val plotData = new ScatterPlotData(r1.resistance, r1.voltage)

    override protected def render: DiffNode[Element, Node] = <.div(Challenge.textAndEx(
      <.div(
        Common.marked(
          s"""
            |### Ohm's Law with a Current Source
            |
            |This time we've changed the circuit so you have a Current Source: a component that will emit the same
            |current regardless of what is connected to it.
            |
            |Again, adjust the resistance R<sub>1</sub>, and this time see what happens to the voltage across the
            |resistor.
            |
            |""".stripMargin),
        completion match {
          case Complete(_, _) =>
            <.div(
              Common.marked(
                """
                  |`V = I R`, so if the current is fixed, voltage will rise linearly with resistance.
                  |
                  |It is, however, rare for real devices to be powered by an "ideal current source".
                  |In most devices, the power supply puts out a fixed voltage (e.g. batteries or 5V USB power).
                  |""".stripMargin),
              nextButton()
            )
          case _ => <.div()
        }
      )
    )(
      <.div(
        circuit,
        ScatterPlot(600, 300, "Resistance", "Voltage", (d) => r1.resistance.stringify(d), (d) => r1.voltage.stringify(d), 1000, 10).plot(plotData.dataValues())
      )
    ))
  }

}
