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

object NandGateLogic extends ExerciseStage {

  given wireCol:Wire.ColouringRule = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val a1:LogicInput = new LogicInput(100 ->350, East, name="A")({ v => a2.value = v; onUpdate() })
  val b1:LogicInput = new LogicInput(100 ->450, East, name="B")({ v => b2.value = v; onUpdate() })
  val nand = new NandGate(200 -> 400, East)
  val out = new LogicProbe(300 -> 400, East)

  val a2:LogicInput = new LogicInput(100 ->150, East, name="A")({ v => a1.value = v; onUpdate() })
  val b2:LogicInput = new LogicInput(100 ->250, East, name="B")({ v => b1.value = v; onUpdate() })
  val and = new AndGate(200 -> 200, East)
  val not = new NotGate(300 -> 200)
  val out2 = new LogicProbe(400 -> 200, East)

  val wires:Seq[Wire] = Seq(
    (a1.t -> nand.ta).wireVia(nand.ta.x -> a1.t.y),
    (b1.t -> nand.tb).wireVia(nand.tb.x -> b1.t.y),
    (nand.out -> out.t).wire,

    (a2.t -> and.ta).wireVia(and.ta.x -> a2.t.y),
    (b2.t -> and.tb).wireVia(and.tb.x -> b2.t.y),
    (and.out -> not.in).wire,
    (not.out -> out2.t).wire
  )

  val circuit = new Circuit(Seq(a1, b1, nand, out) ++ Seq(a2, b2, and, not, out2) ++ wires, 600, 600)
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
           |### NAND gate in logic
           |
           |Let's take an AND gate and put a NOT gate on its output.
           |
           |This produces a "NAND" gate. The logic circuit symbol for it is underneath -- you'll see the negation
           |circle gets added to the AND gate's output.
           |
           |Let's fill in the truth table.
           |
           |${TruthTable(Seq("A", "B"), Seq("Output"), truthTable, a1.value.toSeq ++ b1.value.toSeq).htmlString}
           |""".stripMargin
      ), if (isComplete) <.div(
        Common.marked(
          s"""
             | If you look at the truth table, you'll notice that the output is `0` if A or B is `0`.
             |
             | In other words, `~(AB)` = `(A' + B')`. This is one of [De Morgan's Laws](https://en.wikipedia.org/wiki/De_Morgan%27s_laws)
             |""".stripMargin
        ), nextButton()
      )else <.div()
    )
  )(<.div(
    circuit
  )))

}
