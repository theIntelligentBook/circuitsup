package circuitsup.analog

import circuitsup.Common
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.<
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Open}
import com.wbillingsley.wren.{Circuit, ConstraintPropagator, CurrentSource, EquationConstraint, Ground, Resistor, Terminal, Value, ValueLabel, ValueSlider, VoltageSource, Wire}
import com.wbillingsley.wren.Orientation.{East, North, South}
import org.scalajs.dom.{Element, Node}

object KVL {

  import Analog.{nextButton, onCompletionUpdate}
  import com.wbillingsley.wren.Wire._

  object page1 extends ExerciseStage {

    var completion: Challenge.Completion = Open

    val cs = new CurrentSource((150, 200), North, Some(0.001))
    val r1 = new Resistor((350, 160), South)
    val tout = new Terminal((350, 200), i = Some(0))
    val r2 = new Resistor((350, 240), South)
    val gnd = Ground((150, 300))

    val components = Seq(cs, r1, tout, r2, gnd)
    val wires:Seq[Wire] = Seq(
      (cs.t2 -> r1.t1) wireVia (150 -> 100, 350 -> 100),
      (cs.t1 -> gnd.terminal).wire,
      (gnd.terminal -> r2.t2) wireVia (350 -> 300),
      (r2.t1 -> tout).wire,
      (tout -> r1.t2).wire
    )

    val labels = Seq(
      new ValueLabel("R" -> "1", r1.resistance, pos=(370, 160)),
      new ValueLabel("R" -> "2", r2.resistance, pos=(370, 240)),
      new ValueLabel("I" -> " ", cs.current, pos=(180, 200)),
      new ValueLabel("V" -> "cc", cs.voltage, pos=(110, 200), "right", symbol=Seq(ValueLabel.voltageMarkers((110, 100), (110, 300))), colouringRule = () => if (checkCompletion) "green" else ""),
      new ValueLabel("V" -> "1", r1.voltage, pos=(500, 160), symbol=Seq(ValueLabel.voltageMarkers((500, 130), (500, 180)))),
      new ValueLabel("V" -> "2", r2.voltage, pos=(500, 240), symbol=Seq(ValueLabel.voltageMarkers((500, 210), (500, 260)))),
      new ValueSlider(r1.resistance, (370, 170), East, "1000", "10000", "1000")(() => onUpdate()),
      new ValueSlider(r2.resistance, (370, 250), East, "1000", "10000", "1000")(() => onUpdate())
    )

    val circuit = Circuit(components ++ wires ++ labels, width=600, height=400)
    val propagator = ConstraintPropagator(circuit.components.flatMap(_.constraints))


    def checkCompletion:Boolean = cs.voltage.is(5, 0.1)

    def onUpdate():Unit = {
      propagator.clearCalculations()
      propagator.resolve()
      if (checkCompletion) {
        completion = Complete(Some(1), None)
        onCompletionUpdate()
      } else {
        rerender()
      }
    }


    override protected def render: DiffNode[Element, Node] = <.div(Challenge.textAndEx(
        <.div(
          Common.marked(
          s"""
            |### Kirchhoff's Voltage Law
            |
            |Kirchhoff's voltage law is that if you sum the potential differences around any loop in the circuit, you
            |will get zero.
            |
            |In this circuit, we've got a current source on the left hand side two resistors on the right hand side.
            |KCL tells us that the current through the two resistors is the same (the current the current source is putting out).
            |Ohm's law tells us what the voltage across each resistor must be.
            |
            |KVL tells us that the voltage across the current source must match the total voltage across the two resistors.
            |If we come down 5V through the resistors, we have to go up 5V through the current source. The sum of
            |potential differences as we go around the circuit must be zero.
            |
            |If we take a clockwise loop around this circuit, first we go up through V<sub>cc</sub>, then we come *down*
            |through V<sub>1</sub> and V<sub>2</sub>. You'll notice that going clockwise, we pass through the negative terminal
            |of the current source first (going from lower to higher potential) but on the right hand side we pass down through the positive side of V<sub>1</sub> and
            |V<sub>2</sub> (going from higher to lower potential). So, our equation of the changes in potential as we move around the loop is:
            |
            || V<sub>cc</sub> - V<sub>1</sub> - V<sub>2</sub> = 0 |
            || --- |
            || ${cs.voltage.stringify("?")} - ${r1.voltage.stringify("?")} - ${r2.voltage.stringify("?")} = 0  |
            |
            |Your task is to set the voltage across the current source to 5V. Notice how the equation always holds.
            |""".stripMargin
          ),
          if (isComplete) nextButton() else <.div()
        )
      )(circuit)
    )
  }

  object page2 extends ExerciseStage {
    var completion: Challenge.Completion = Open

    val vs = new VoltageSource((150, 150), North, Some(5))
    val r1 = new Resistor((300, 150), South)
    val t1 = new Terminal((300, 100), Some(0))
    val t2 = new Terminal((300, 200), Some(0))
    val r2 = new Resistor((500, 150), South)
    val gnd = Ground((150, 200))

    val components = Seq(vs, r1, r2, t1, t2, gnd)
    val wires:Seq[Wire] = Seq(
      (vs.t2 -> t1) wireVia (150 -> 100),
      (t1 -> r1.t1).wire,
      (t1 -> r2.t1).wireVia(500 -> 100),
      (vs.t1 -> gnd.terminal).wire,
      (gnd.terminal -> t2).wire,
      (r1.t2 -> t2).wire,
      (r2.t2 -> t2).wireVia(500 -> 200)
    )

    val labels = Seq(
      new ValueLabel("R" -> "1", r1.resistance, pos=(320, 150)),
      new ValueLabel("R" -> "2", r2.resistance, pos=(520, 150)),
      new ValueLabel("I" -> " ", vs.t1.current, pos=(160, 80), symbol = Seq(ValueLabel.currentArrow(160 -> 90, East)), colouringRule = () => if (checkCompletion) "green" else ""),
      new ValueLabel("V" -> "cc", vs.voltage, pos=(110, 150), "right"),
      new ValueLabel("I" -> "1", r1.t1.current, pos=(270, 150), "right", symbol = Seq(ValueLabel.currentArrow(280 -> 150, South))),
      new ValueLabel("I" -> "2", r2.t1.current, pos=(400, 80), "middle", symbol = Seq(ValueLabel.currentArrow(400 -> 90, East))),
      new ValueSlider(r1.resistance, (320, 160), East, "1000", "10000", "1000")(() => onUpdate()),
      new ValueSlider(r2.resistance, (520, 160), East, "1000", "10000", "1000")(() => onUpdate())
    )

    val circuit = Circuit(components ++ wires ++ labels, width=800, height=400)
    val propagator = ConstraintPropagator(circuit.components.flatMap(_.constraints))


    def checkCompletion:Boolean = vs.t1.current.is(0.0015, 0.0001)

    def onUpdate():Unit = {
      propagator.clearCalculations()
      propagator.resolve()
      if (checkCompletion) {
        completion = Complete(Some(1), None)
        onCompletionUpdate()
      } else {
        rerender()
      }
    }


    override protected def render: DiffNode[Element, Node] = <.div(Challenge.textAndEx(
      <.div(
        Common.marked(
          s"""
             |### KVL, Ohm's, and KVL together.
             |
             |Kirchhoff's Voltage Law works when there are two loops in the circuit as well. The voltage V<sub>cc</sub>
             |must be the same as the Voltage across R<sub>1</sub> and it must also match the voltage across R<sub>2</sub>.
             |
             |Ohm's Law can then be used to work out the current through R<sub>1</sub> and R<sub>2</sub>
             |
             |* I<sub>1</sub> = V<sub>cc</sub> / R<sub>1</sub><br />
             |  ${r1.t1.current.stringify("?")} = ${vs.voltage.stringify("?")} / ${r1.resistance.stringify("?")}
             |
             |* I<sub>1</sub> = V<sub>cc</sub> / R<sub>1</sub><br />
             |  ${r2.t1.current.stringify("?")} = ${vs.voltage.stringify("?")} / ${r2.resistance.stringify("?")}
             |
             |Once we have those two currents, KCL means the current coming out of the voltage source must be the sum
             |of the currents through those two resistors.
             |
             |* I = I<sub>1</sub> + I<sub>2</sub><br />
             |  ${vs.t1.current.stringify("?")} = ${r1.t1.current.stringify("?")} + ${r2.t1.current.stringify("?")}
             |
             |Your task is to set the current out of the Voltage Source (I) to 1.5mA. Notice how the equation always holds.
             |""".stripMargin
        ),
        if (isComplete) nextButton() else <.div()
      )
    )(circuit)
    )
  }

  object page3 extends ExerciseStage {

    var completion: Challenge.Completion = Open

    val cs = new VoltageSource((150, 200), North, Some(5))
    val r1 = new Resistor((350, 160), South)
    val tout = new Terminal((350, 200), i = Some(0))
    val r2 = new Resistor((350, 240), South)
    val gnd = Ground((150, 300))

    val components = Seq(cs, r1, tout, r2, gnd)
    val wires:Seq[Wire] = Seq(
      (cs.t2 -> r1.t1) wireVia (150 -> 100, 350 -> 100),
      (cs.t1 -> gnd.terminal).wire,
      (gnd.terminal -> r2.t2) wireVia (350 -> 300),
      (r2.t1 -> tout).wire,
      (tout -> r1.t2).wire
    )

    val labels = Seq(
      new ValueLabel("R" -> "1", r1.resistance, pos=(370, 160)),
      new ValueLabel("R" -> "2", r2.resistance, pos=(370, 240)),
      new ValueLabel("I" -> " ", cs.t1.current, pos=(180, 200), symbol=Seq(ValueLabel.currentArrow((110, 100), North))),
      new ValueLabel("V" -> "cc", cs.voltage, pos=(110, 200), "right"),
      new ValueLabel("V" -> "1", r1.voltage, pos=(500, 160), symbol=Seq(ValueLabel.voltageMarkers((500, 130), (500, 180)))),
      new ValueLabel("V" -> "2", r2.voltage, pos=(500, 240), symbol=Seq(ValueLabel.voltageMarkers((500, 210), (500, 260))), colouringRule = () => if (checkCompletion) "green" else ""),
      new ValueSlider(r1.resistance, (370, 170), East, "1000", "10000", "1000")(() => onUpdate()),
      new ValueSlider(r2.resistance, (370, 250), East, "1000", "10000", "1000")(() => onUpdate())
    )

    val totalR = new Value("Ω")

    val circuit = Circuit(components ++ wires ++ labels, width=600, height=400)
    val propagator = ConstraintPropagator(circuit.components.flatMap(_.constraints)
      :+ EquationConstraint("Resistors in Series", totalR, Seq(r1.resistance, r2.resistance), () => r1.resistance + r2.resistance)
      :+ EquationConstraint("Ohm's Law", cs.t1.current, Seq(cs.voltage, totalR), () => cs.voltage / totalR)
    )


    def checkCompletion:Boolean = r2.voltage.is(2, 0.01)

    def onUpdate():Unit = {
      propagator.clearCalculations()
      propagator.resolve()
      if (checkCompletion) {
        completion = Complete(Some(1), None)
        onCompletionUpdate()
      } else {
        rerender()
      }
    }


    override protected def render: DiffNode[Element, Node] = <.div(Challenge.textAndEx(
      <.div(
        Common.marked(
          s"""
             |### Voltage Dividers
             |
             |Let's go back to our example with two resistors "in series", but this time use a voltage source instead
             |of a current source.
             |
             |This circuit is called a *voltage divider* because it turns out the voltage across R<sub>1</sub> and
             |R<sub>2</sub> splits in proportion to their resistances.
             |
             |As a little intuition for the circuit:
             |
             |* KVL tells us that V<sub>cc</sub> = V<sub>1</sub> + V<sub>2</sub>
             |* KCL tells us that the current through the two resistors is the same
             |* Ohms law means that V<sub>cc</sub> = I R<sub>1</sub> + I R<sub>2</sub>
             |
             |So, the bigger R<sub>1</sub> is, the bigger its share of the voltage is.
             |
             |Your task is to set the voltage across R<sub>2</sub> to 2V
             |""".stripMargin
        ),
        if (isComplete) nextButton() else <.div()
      )
    )(circuit)
    )
  }


}
