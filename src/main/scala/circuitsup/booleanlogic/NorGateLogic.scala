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

object NorGateLogic extends ExerciseStage {

  implicit val wireCol = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val a1:LogicInput = new LogicInput(100 ->350, East, name="A")({ v => a2.value = v; onUpdate() })
  val b1:LogicInput = new LogicInput(100 ->450, East, name="B")({ v => b2.value = v; onUpdate() })
  val nor = new NorGate(200 -> 400, East)
  val out = new LogicProbe(300 -> 400, East)

  val a2:LogicInput = new LogicInput(100 ->150, East, name="A")({ v => a1.value = v; onUpdate() })
  val b2:LogicInput = new LogicInput(100 ->250, East, name="B")({ v => b1.value = v; onUpdate() })
  val or = new OrGate(200 -> 200, East)
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
           |### NOR gate in logic
           |
           |Now let's take an OR gate and put a NOT gate on its output.
           |
           |This produces a "NOR" gate. The logic circuit symbol for it is underneath -- you'll see the negation
           |circle gets added to the OR gate's output.
           |
           |Let's fill in the truth table by setting the inputs in the circuit on the right.
           |
           |${TruthTable(Seq("A", "B"), Seq("Output"), truthTable, a1.value.toSeq ++ b1.value.toSeq).htmlString}
           |""".stripMargin
      ), if (isComplete) <.div(
        Common.marked(
          s"""
             | If you look at the truth table, you'll notice that the output is only `0` if A and B are both `0`.
             |
             | In other words, `~(A + B)` = `(A'B')`. This is the other of [De Morgan's Laws](https://en.wikipedia.org/wiki/De_Morgan%27s_laws)
             |""".stripMargin
        ), nextButton()
      )else <.div()
    )
  )(<.div(
    circuit
  )))

}
