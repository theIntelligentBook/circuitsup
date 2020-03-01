package circuitsup.booleanlogic

import circuitsup.templates.ExerciseStage
import BooleanTopic.nextButton
import BooleanTopic.onCompletionUpdate
import circuitsup.Common
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.<
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Open}
import com.wbillingsley.wren.Orientation.{East, North, South}
import com.wbillingsley.wren.{AndGate, Circuit, ConstraintPropagator, Ground, LogicInput, LogicProbe, NMOSSwitch, PMOSSwitch, Terminal, TruthTable, ValueLabel, VoltageSource, Wire}
import org.scalajs.dom.{Element, Node}
import Wire._

object AndGateLogic extends ExerciseStage {

  implicit val wireCol = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val a1:LogicInput = new LogicInput(100 ->150, East, name="A")({ _ => onUpdate() })
  val b1:LogicInput = new LogicInput(100 ->250, East, name="B")({ _ => onUpdate() })

  val and = new AndGate(200 -> 200, East)

  val out = new LogicProbe(300 -> 200, East)

  val wires:Seq[Wire] = Seq(
    (a1.t -> and.ta).wireVia(and.ta.x -> a1.t.y),
    (b1.t -> and.tb).wireVia(and.tb.x -> b1.t.y),
    (and.out -> out.t).wire
  )

  val circuit = new Circuit(Seq(a1, b1, and, out) ++ wires, 600, 600)
  val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
  propagator.resolve()

  def checkCompletion:Boolean = truthTable.size >= 4

  var truthTable = Map.empty[Seq[Boolean], Seq[Boolean]]

  def stringify(o:Option[Boolean]):String = o match {
    case Some(true) => "1"
    case Some(false) => "0"
    case _ => "?"
  }

  def onUpdate():Unit = {
    propagator.clearCalculations()
    propagator.resolve()

    for {
      a <- a1.value
      b <- b1.value
      out <- out.value
    } truthTable = truthTable.updated(Seq(a, b), Seq(out))

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
           |### AND gate in logic
           |
           |Let's replace the circuit with the logic circuit representation.
           |
           |We can also just write the equation. There are a few different ways of doing this, depending on the
           |notational system you use.
           |
           |* <code>A&and;B</code> - the "conjunction" operator you'd see on many modern mathematical courses. A common mnemonic is to think of it like the "n" in "and".
           |* <code>A&middot;B</code> or just `AB` - engineers like to write it like a multiplication. Remember, anything times 0 is 0.
           |  The mid-dot notation <code>A&middot;B</code> is also the one used in [Principia Mathematica](https://en.wikipedia.org/wiki/Principia_Mathematica).
           |* <code>A&amp;B</code>  - the *bitwise AND* operator in many programming languages.
           |
           |Let's fill in the truth table.
           |
           |${TruthTable(Seq("A", "B"), Seq("A·B"), truthTable, a1.value.toSeq ++ b1.value.toSeq).htmlString}
           |
           |It's time to click the logic input buttons to toggle them!
           |""".stripMargin
      ), if (isComplete) <.div(
        Common.marked(
          s"""
             | You might notice that on the logic gate, we showed the output going to zero if either input was low *even if the other
             | input wasn't set*. That's to mimic the circuit behaviour - of our MOSFET circuit. Even if one input
             | was totally disconnected, putting a zero into the other input would produce a zero output.
             |""".stripMargin
        ), nextButton()
      )else <.div()
    )
  )(<.div(
    circuit
  )))

}
