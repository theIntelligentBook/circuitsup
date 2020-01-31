package circuitsup.analog

import circuitsup.Common
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.<
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Open}
import com.wbillingsley.wren.{Circuit, ConstraintPropagator, EquationConstraint, Ground, LogicProbe, Resistor, Terminal, Value, ValueLabel, ValueSlider, VoltageSource, Wire}
import com.wbillingsley.wren.Orientation.{East, North, South}
import org.scalajs.dom.{Element, Node}
import Analog.{nextButton, onCompletionUpdate}
import com.wbillingsley.wren.Wire._

object GoingDigital {

  object page1 extends ExerciseStage {

    var completion: Challenge.Completion = Open

    val cs = new VoltageSource((150, 200), North, Some(5))
    val r1 = new Resistor((350, 160), South)
    val tout = new Terminal((350, 200), i = Some(0))
    val r2 = new Resistor((350, 240), South)
    val gnd = Ground((150, 300))
    val probe = new LogicProbe((400, 200))

    var done1 = false
    var done0 = false
    var doneUnk = false

    val components = Seq(cs, r1, tout, r2, gnd, probe)
    val wires:Seq[Wire] = Seq(
      (cs.t2 -> r1.t1) wireVia (150 -> 100, 350 -> 100),
      (cs.t1 -> gnd.terminal).wire,
      (gnd.terminal -> r2.t2) wireVia (350 -> 300),
      (r2.t1 -> tout).wire,
      (tout -> r1.t2).wire,
      (tout -> probe.t).wire
    )

    val labels = Seq(
      new ValueLabel("R" -> "1", r1.resistance, pos=(320, 160), "right"),
      new ValueLabel("R" -> "2", r2.resistance, pos=(320, 240), "right"),
      new ValueLabel("V" -> "cc", cs.voltage, pos=(110, 200), "right"),
      new ValueLabel("V" -> "2", r2.voltage, pos=(370, 240), symbol=Seq(ValueLabel.voltageMarkers((370, 220), (370, 270)))),
      new ValueSlider(r1.resistance, (220, 170), East, "1000", "10000", "1000")(() => onUpdate()),
      new ValueSlider(r2.resistance, (220, 250), East, "1000", "10000", "1000")(() => onUpdate())
    )

    val totalR = new Value("Ω")

    val circuit = Circuit(components ++ wires ++ labels, width=600, height=400)
    val propagator = ConstraintPropagator(circuit.components.flatMap(_.constraints)
      :+ EquationConstraint("Resistors in Series", Seq(totalR -> (() => r1.resistance + r2.resistance)))
      :+ EquationConstraint("Ohm's Law", Seq(cs.t1.current -> (() => cs.voltage / totalR)))
    )


    def checkCompletion:Boolean = {
      if (r2.voltage <=1.5) done0 = true
      else if (r2.voltage >= 3.5) done1 = true
      else doneUnk = true

      done0 && done1 && doneUnk
    }

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
             |### Logic Levels
             |
             |Suppose we have a component called a *Logic Probe*. This component is going to read `1` if its voltage
             |is above 3.5V and will read `0` if its input voltage is below 1.5V. The voltages are measured compared
             |to the "reference ground" (the little downwards-facing triangle in the bottom left).
             |
             |Your task is to set the logic probe into all three different states.
             |""".stripMargin
        ),
        if (isComplete) nextButton() else <.div()
      )
    )(circuit)
    )
  }


  object page2 extends ExerciseStage {

    var completion: Challenge.Completion = Open

    val cs = new VoltageSource((150, 200), North, Some(5))
    val r1 = new Resistor((350, 160), South, r=Some(5000))
    val tout = new Terminal((350, 200), i = Some(0))
    val r2 = new Resistor((350, 240), South)
    val gnd = Ground((150, 300))
    val probe = new LogicProbe((400, 200))

    var done1 = false
    var done0 = false

    val components = Seq(cs, r1, tout, r2, gnd, probe)
    val wires:Seq[Wire] = Seq(
      (cs.t2 -> r1.t1) wireVia (150 -> 100, 350 -> 100),
      (cs.t1 -> gnd.terminal).wire,
      (gnd.terminal -> r2.t2) wireVia (350 -> 300),
      (r2.t1 -> tout).wire,
      (tout -> r1.t2).wire,
      (tout -> probe.t).wire
    )

    val labels = Seq(
      new ValueLabel("R" -> "1", r1.resistance, pos=(320, 160), "right"),
      new ValueLabel("R" -> "2", r2.resistance, pos=(320, 240), "right"),
      new ValueLabel("V" -> "cc", cs.voltage, pos=(110, 200), "right"),
      new ValueLabel("V" -> "2", r2.voltage, pos=(370, 240), symbol=Seq(ValueLabel.voltageMarkers((370, 220), (370, 270)))),
      new ValueSlider(r2.resistance, (220, 250), East, "1000", "100000", "99000")(() => onUpdate())
    )

    val totalR = new Value("Ω")

    val circuit = Circuit(components ++ wires ++ labels, width=600, height=400)
    val propagator = ConstraintPropagator(circuit.components.flatMap(_.constraints)
      :+ EquationConstraint("Resistors in Series", Seq(totalR -> (() => r1.resistance + r2.resistance)))
      :+ EquationConstraint("Ohm's Law", Seq(cs.t1.current -> (() => cs.voltage / totalR)))
    )


    def checkCompletion:Boolean = {
      if (r2.voltage <=1.5) done0 = true
      else if (r2.voltage >= 3.5) done1 = true

      done0 && done1
    }

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
             |### Static Discipline
             |
             |In digital circuits, the "static discipline" is that we never want a logic component's input to be in
             |that "forbidden zone" between its logic-1 level and its logic-0 level.
             |
             |In this example, I've fixed R<sub>1</sub> at 5kΩ. You can still control R<sub>2</sub>, but now it only has
             |two positions: 1kΩ or 100kΩ.
             |
             |Toggle the slider and see how this means we'll only get a 1 or a 0 on the logic probe.
             |""".stripMargin
        ),
        if (isComplete) Common.marked(
          """
            |Our next problem, however, is how to get a component whose resistance we can toggle electronically to be
            |very very low or very very high. But that's a story for another time, when we'll introduce MOSFETs.
            |
            |**Congratulations** on making it to the end of this lesson.
            |""".stripMargin)
        else <.div()
      )
    )(circuit)
    )
  }
}
