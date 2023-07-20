package circuitsup.booleanlogic

import circuitsup.templates.ExerciseStage
import BooleanTopic.nextButton
import BooleanTopic.onCompletionUpdate
import circuitsup.Common
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.<
import com.wbillingsley.veautiful.doctacular.Challenge
import com.wbillingsley.veautiful.doctacular.Challenge.{Complete, Open}
import com.wbillingsley.wren.Orientation.{East, North, South}
import com.wbillingsley.wren.{AndGate, Circuit, ConstraintPropagator, Ground, LogicInput, LogicProbe, NMOSSwitch, OrGate, PMOSSwitch, Terminal, TruthTable, ValueLabel, VoltageSource, Wire}
import org.scalajs.dom.{Element, Node}
import Wire._

object OrGateLogic extends ExerciseStage {

  given wireCol:Wire.ColouringRule = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val a1:LogicInput = new LogicInput(100 ->150, East, name="A")({ _ => onUpdate() })
  val b1:LogicInput = new LogicInput(100 ->250, East, name="B")({ _ => onUpdate() })

  val or = new OrGate(200 -> 200, East)

  val out = new LogicProbe(300 -> 200, East)

  val wires:Seq[Wire] = Seq(
    (a1.t -> or.ta).wireVia(or.ta.x -> a1.t.y),
    (b1.t -> or.tb).wireVia(or.tb.x -> b1.t.y),
    (or.out -> out.t).wire
  )

  val circuit = new Circuit(Seq(a1, b1, or, out) ++ wires, 600, 600)
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

  override protected def render = <.div(Challenge.textAndEx(
    <.div(
      Common.marked(
        s"""
           |### OR gate in logic
           |
           |Let's replace the circuit with the logic circuit representation.
           |
           |We can also just write the equation. There are a few different ways of doing this, depending on the
           |notational system you use.
           |
           |* <code>A&or;B</code> - this is the "alternation" or "disjunction" operator from [Principia Mathematica](https://en.wikipedia.org/wiki/Principia_Mathematica).
           |  It's also what you'd see on mathematical logic courses. Trivia: the modern conjunction (and) symbol <code>&and;</code> comes from it being an upside-down <code>&or;</code>.
           |* <code>A+B</code> - engineers like to write it like an addition. Trivia: this notation is also useful for remembering the *precendce* rules. In
           |  <code>A+B&middot;C</code>, the conjunction (and) takes precedence, so it's <code>A+(B&middot;C)</code>. That'll be useful because it lets us write things such as
           |  `AB' + A'B` if we want to consider only the cases `10` and `01`.
           |* <code>A|B</code>  - the *bitwise OR* operator in many programming languages.
           |
           |Let's fill in the truth table.
           |
           |${TruthTable(Seq("A", "B"), Seq("A+B"), truthTable, a1.value.toSeq ++ b1.value.toSeq).htmlString}
           |
           |It's time to click the logic input buttons to toggle them!
           |""".stripMargin
      ), if (isComplete) <.div(
        Common.marked(
          s"""
             | It might have been easy to miss, but we've modelled the OR gate so that if one input is left floating, a `1` at the other input can still drive it to `1`.
             | That mimics the MOSFET circuit we saw on the previous page.
             |
             |""".stripMargin
        ), nextButton()
      )else <.div()
    )
  )(<.div(
    circuit
  )))

}
