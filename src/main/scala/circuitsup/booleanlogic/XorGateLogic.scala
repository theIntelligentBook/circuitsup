package circuitsup.booleanlogic

import circuitsup.Common
import circuitsup.booleanlogic.BooleanTopic.{nextButton, onCompletionUpdate}
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.<
import com.wbillingsley.veautiful.doctacular.Challenge
import com.wbillingsley.veautiful.doctacular.Challenge.{Complete, Open}
import com.wbillingsley.wren.Orientation.East
import com.wbillingsley.wren.Wire._
import com.wbillingsley.wren._
import org.scalajs.dom.{Element, Node}

object XorGateLogic extends ExerciseStage {

  given wireCol:Wire.ColouringRule = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val a1:LogicInput = new LogicInput(100 ->150, East, name="A")({ _ => onUpdate() })
  val b1:LogicInput = new LogicInput(100 ->250, East, name="B")({ _ => onUpdate() })
  val nor = new XorGate(200 -> 200, East)
  val out = new LogicProbe(300 -> 200, East)


  val wires:Seq[Wire] = Seq(
    (a1.t -> nor.ta).wireVia(nor.ta.x -> a1.t.y),
    (b1.t -> nor.tb).wireVia(nor.tb.x -> b1.t.y),
    (nor.out -> out.t).wire,
  )

  val circuit = new Circuit(Seq(a1, b1, nor, out) ++ wires, 600, 600)
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
           |### XOR gate in logic
           |
           |Earlier, we met an *inclusive* OR gate. Let's now meet the *exclusive OR* gate XOR.
           |
           |XOR produces `1` if exactly one of A and B are `1`. Or, effectively, A is not equal to B.
           |
           |The circuit symbol is like an OR gate, but with a double curved line on the input side.
           |
           |As with the other gates, there are different notations for writing it down mathematically:
           |
           |* `A⊻B` - like the disjunction operator but with an extra underline. This one tends to be used in mathematics books.
           |* `A⊕B` - where the engineers use `+` for or, they use plus with a circle around it for XOR
           |* `A^B` - in many C-like programming languages, the caret `^` is used as the bitwise XOR operator. We'll avoid this
           |  in these notes because it's too easy to mistake it for the mathematical conjunction symbol `∧` when written on paper.
           |
           |Let's fill in the truth table by setting the inputs in the circuit on the right.
           |
           |${TruthTable(Seq("A", "B"), Seq("Output"), truthTable, a1.value.toSeq ++ b1.value.toSeq).htmlString}
           |""".stripMargin
      ), if (isComplete) <.div(
        Common.marked(
          s"""
             | If we were just to read the lines on the truth table, we'd see that the two rows that are `1` are
             | `AB' + A'B`.
             |
             | We'll also later find that if we XOR multiple inputs together, for instance `A⊕B⊕C`, it'll produce a `1`
             | if there are an *odd* number of `1`s in the input.
             |""".stripMargin
        ), nextButton()
      )else <.div()
    )
  )(<.div(
    circuit
  )))

}
