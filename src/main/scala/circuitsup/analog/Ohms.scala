package circuitsup.analog

import circuitsup.{CircuitsRoute, Common, Router}
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, ^}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Open}
import com.wbillingsley.wren.{Circuit, Constraint, ConstraintPropagator, CurrentSource, Ground, Orientation, Resistor, ValueLabel, ValueSlider, VoltageSource, Wire}
import org.scalajs.dom.{Element, Node}

object Ohms {

  object page1 extends ExerciseStage {

    override def completion: Challenge.Completion = Open

    def onUpdate():Unit = {
      propagator.clearCalculations()
      propagator.resolve()
      /*if (checkCompletion) {
        completion = Complete(Some(1), None)
      }*/

      Router.rerender()
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
    val rs = new ValueSlider(r1.resistance, (320, 160), min="100", max="10000", step="10")(onUpdate=onUpdate)

    val circuit = Circuit(Seq(gnd, vs, vl, r1, rl, il, w1, w2, w3, rs), 600, 400)
    val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))

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
            |(The task will complete when there's enough data values
            |
            |""".stripMargin),
        completion match {
          case Complete(_, _) =>
            <.div(
              <.p(
                "Well done. And if you look at each junction in the circuit, the currents in and out should sum to zero."
              ),
              <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(CircuitsRoute(1, 2)), s"Next")
            )
          case _ => <.div()
        }
      )
    )(
      <.div(
        circuit

      )
    ))
  }

  object page3 extends ExerciseStage {

    override def completion: Challenge.Completion = Open

    def onUpdate():Unit = {
      propagator.clearCalculations()
      propagator.resolve()
      /*if (checkCompletion) {
        completion = Complete(Some(1), None)
      }*/

      Router.rerender()
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
              <.p(
                "Well done. And if you look at each junction in the circuit, the currents in and out should sum to zero."
              ),
              <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(CircuitsRoute(1, 2)), s"Next")
            )
          case _ => <.div()
        }
      )
    )(
      <.div(
        circuit

      )
    ))
  }

}
