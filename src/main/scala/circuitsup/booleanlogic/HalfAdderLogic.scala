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

object HalfAdderLogic extends ExerciseStage {

  implicit val wireCol = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val a1:LogicInput = new LogicInput(100 ->350, East, name="A")({ v => a2.value = v; onUpdate() })
  val b1:LogicInput = new LogicInput(100 ->450, East, name="B")({ v => b2.value = v; onUpdate() })
  val and = new AndGate(200 -> 400, East)
  val carry = new LogicProbe(300 -> 400, East, "Carry")

  val a2:LogicInput = new LogicInput(100 ->150, East, name="A")({ v => a1.value = v; onUpdate() })
  val b2:LogicInput = new LogicInput(100 ->250, East, name="B")({ v => b1.value = v; onUpdate() })
  val xor = new XorGate(200 -> 200, East)
  val result = new LogicProbe(300 -> 200, East, "Result")

  val wires:Seq[Wire] = Seq(
    (a1.t -> and.ta).wireVia(and.ta.x -> a1.t.y),
    (b1.t -> and.tb).wireVia(and.tb.x -> b1.t.y),
    (and.out -> carry.t).wire,

    (a2.t -> xor.ta).wireVia(xor.ta.x -> a2.t.y),
    (b2.t -> xor.tb).wireVia(xor.tb.x -> b2.t.y),
    (xor.out -> result.t).wire
  )

  val circuit = new Circuit(Seq(a1, b1, and, carry) ++ Seq(a2, b2, xor, result) ++ wires, 600, 600)
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
      c <- carry.value
      r <- result.value
    } truthTable = truthTable.updated(Seq(a, b), Seq(c, r))

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
           |### Half adder
           |
           |So far, we've shown logic gates just to show what they do. Now let's do something small but useful
           |
           |In binary, we count using only the digits `0` and `1`. If we suppose we have two "bits", we could count from
           |0 to 3 like so:
           |
           |* `0`: `00`
           |* `1`: `01`
           |* `2`: `10`
           |* `3`: `11`
           |
           |Let's say we have two one-bit numbers `A` and `B`. How can we implement addition of these bits?
           |
           |The circuit on the right is a *half-adder* that can add two bits. You'll see we have two circuits producing
           |two outputs, that I've labelled "Carry" and "Result". This is because `1` + `1` = `10` - we need two bits
           |to store the output.
           |
           |Toggle the inputs to try out the different possibilites and hopefully the result will make sense in the truth
           |table for the *Carry* and *Result* bits below.
           |
           |${TruthTable(Seq("A", "B"), Seq("Carry", "Result"), truthTable, a1.value.toSeq ++ b1.value.toSeq).htmlString}
           |""".stripMargin
      ), if (isComplete) <.div(
        Common.marked(
          s"""
             | As should be apparent in the circuit diagram, Result = `A⊕B` and Carry = `A·B`
             |""".stripMargin
        ), nextButton()
      )else <.div()
    )
  )(<.div(
    circuit
  )))

}
