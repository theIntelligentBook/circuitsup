package circuitsup.booleanlogic

import circuitsup.Common
import circuitsup.booleanlogic.BooleanTopic.{nextButton, onCompletionUpdate}
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.<
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Open}
import com.wbillingsley.wren.Orientation.East
import com.wbillingsley.wren.Wire._
import com.wbillingsley.wren._
import org.scalajs.dom.{Element, Node}

object XnorGateLogic extends ExerciseStage {

  given wireCol:Wire.ColouringRule = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val a1:LogicInput = new LogicInput(100 ->350, East, name="A")({ v => a2.value = v; onUpdate() })
  val b1:LogicInput = new LogicInput(100 ->450, East, name="B")({ v => b2.value = v; onUpdate() })
  val nor = new XnorGate(200 -> 400, East)
  val out = new LogicProbe(300 -> 400, East)

  val a2:LogicInput = new LogicInput(100 ->150, East, name="A")({ v => a1.value = v; onUpdate() })
  val b2:LogicInput = new LogicInput(100 ->250, East, name="B")({ v => b1.value = v; onUpdate() })
  val or = new XorGate(200 -> 200, East)
  val not = new NotGate(300 -> 200)
  val out2 = new LogicProbe(400 -> 200, East)

  val wires:Seq[Wire] = Seq(
    (a1.t -> nor.ta).wireVia(nor.ta.x -> a1.t.y),
    (b1.t -> nor.tb).wireVia(nor.tb.x -> b1.t.y),
    (nor.out -> out.t).wire,

    (a2.t -> or.ta).wireVia(or.ta.x -> a2.t.y),
    (b2.t -> or.tb).wireVia(or.tb.x -> b2.t.y),
    (or.out -> not.in).wire,
    (not.out -> out2.t).wire
  )

  val circuit = new Circuit(Seq(a1, b1, nor, out) ++ Seq(a2, b2, or, not, out2) ++ wires, 600, 600)
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
           |### XNOR gate in logic
           |
           |If there's an XOR gate, what do we get if we put a NOT gate on its output? An XNOR gate.
           |
           |The logic circuit symbol for it is underneath -- you'll see the negation
           |circle gets added to the XOR gate's output to form an XNOR gate.
           |
           |This will output `1` if A and B are both `1` or if they are both `0`.
           |
           |Mathematically, there are a few notations for something like XNOR:
           |
           |* `A⊙B` - where engineers circled the plus for XOR, they circle the middot for XNOR.
           |
           |Most of the other notations are from propositional logic (and make less intuitive sense for bits):
           |
           |* `A iff B` - this one is read "if and only if". The statement *B is true if and only if A is true* is only true if A and B have the same value.
           |*  `A↔B` or `A⇔B` - bidirectional implication arrows can also be used to show "if and only if"
           |* `A≡B`,  - A *is equivalent* to B
           |
           |Let's fill in the truth table by setting the inputs in the circuit on the right.
           |
           |${TruthTable(Seq("A", "B"), Seq("Output"), truthTable, a1.value.toSeq ++ b1.value.toSeq).htmlString}
           |""".stripMargin
      ), if (isComplete) <.div(
        Common.marked(
          s"""
             | It's probably less common to see an XNOR gate. Many programming languages do not have a bitwise XNOR
             | operator. For instance in C-like languages you have to write `~(A^B)` to perform a bitwise XNOR.
             |""".stripMargin
        ), nextButton()
      )else <.div()
    )
  )(<.div(
    circuit
  )))

}
