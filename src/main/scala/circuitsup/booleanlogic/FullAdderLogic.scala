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

object FullAdderLogic extends ExerciseStage {

  given wireCol:ColouringRule = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val a1:LogicInput = new LogicInput(100 ->125, East, name="A")({ v => a2.value = v; onUpdate() })
  val b1:LogicInput = new LogicInput(100 ->175, East, name="B")({ v => b2.value = v; onUpdate() })

  val and1 = new AndGate(200 -> 150, East)
  val and2 = new AndGate(300 -> 225, East)
  val or = new OrGate(400 -> 200, East)
  val carry = new LogicProbe(450 -> 200, East, "Carry")

  val c2:LogicInput = new LogicInput(100 ->225, East, name="Cin")({ _ => onUpdate() })
  val a2:LogicInput = new LogicInput(100 ->325, East, name="A")({ v => a1.value = v; onUpdate() })
  val b2:LogicInput = new LogicInput(100 ->375, East, name="B")({ v => b1.value = v; onUpdate() })
  val xor1 = new XorGate(200 -> 350, East)
  val xor2 = new XorGate(300 -> 300, East)
  val result = new LogicProbe(450 -> xor2.out.y, East, "Result")

  val wires:Seq[Wire] = Seq(
    (a1.t -> and1.ta).wireVia(and1.ta.x -> a1.t.y),
    (b1.t -> and1.tb).wireVia(and1.tb.x -> b1.t.y),

    (a2.t -> xor1.ta).wireVia(xor1.ta.x -> a2.t.y),
    (b2.t -> xor1.tb).wireVia(xor1.tb.x -> b2.t.y),
    (xor1.out -> xor2.tb).wireVia(xor2.tb.x -> xor1.out.y),
    (c2.t -> xor2.ta).wireVia(c2.t.x -> xor2.ta.y),
    (xor2.out -> result.t).wire,

    (xor1.out -> and2.tb).wireVia(xor1.out.x -> and2.tb.y),
    (c2.t -> and2.ta).wireVia(c2.t.x -> and2.ta.y),
    (and1.out -> or.ta).wireVia(and1.out.x -> or.ta.y),
    (and2.out -> or.tb).wireVia(or.ta.x -> and2.out.y),
    (or.out -> carry.t).wire,

  )

  val circuit = new Circuit(Seq(a1, b1, and1, and2, or, carry) ++ Seq(a2, b2, c2, xor1, xor2, result) ++ wires, 600, 600)
  val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
  propagator.resolve()

  def checkCompletion:Boolean = truthTable.size >= 8

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
      cin <- c2.value
      c <- carry.value
      r <- result.value
    } truthTable = truthTable.updated(Seq(a, b, cin), Seq(c, r))

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
           |### Full adder
           |
           |If we're trying to add a binary number, our half adder is only useful for the least significant bit. All
           |the other columns could have a carry coming *in* from the column to their right.
           |
           |For example, in `01 + 01` when working out the left-hand column we have to take into account the carry from
           |the `1+1` in the right-hand column.
           |
           |Effectively, a full-adder has to add three bits: *A*, *B*, and *Carry-in*, to produce two bits: *Result* and
           |*Carry-out*.
           |
           |The circuitry on the right hand-side does this. Again, we've duplicated the A and B inputs to try to avoid
           |too many crossing wires.
           |
           |Toggle the inputs to fill out the truth table below (there are now eight possibilities).
           |
           |${TruthTable(Seq("A", "B", "Cin"), Seq("Carry", "Result"), truthTable, a1.value.toSeq ++ b1.value.toSeq ++ c2.value.toSeq).htmlString}
           |""".stripMargin
      ), if (isComplete) <.div(
        Common.marked(
          s"""
             | As the circuit gets more complicated, it's possibly easier to think of it just in terms of the equations
             |
             | * Result = A⊕B⊕C<sub>in</sub>
             | * C<sub>out</sub> = AB + (A⊕B)·C<sub>in</sub>
             |""".stripMargin
        ), nextButton()
      )else <.div()
    )
  )(<.div(
    circuit
  )))

}
