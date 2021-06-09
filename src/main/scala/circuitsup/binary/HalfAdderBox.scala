package circuitsup.binary

import circuitsup.Common
import circuitsup.binary.BinaryTopic.{nextButton, onCompletionUpdate}
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, ^}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Open}
import com.wbillingsley.wren.Orientation.East
import com.wbillingsley.wren.{AndGate, Binary, Circuit, ConstraintPropagator, HalfAdder, LogicInput, LogicProbe, TruthTable, Wire, XorGate}
import org.scalajs.dom.{Element, Node}
import Wire._

object HalfAdderBox extends ExerciseStage {

  given wireCol:ColouringRule = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val a1:LogicInput = new LogicInput(100 ->50, East, name="A")({ _ => onUpdate() })
  val b1:LogicInput = new LogicInput(100 ->150, East, name="B")({ _ => onUpdate() })
  val adder = new HalfAdder(200 -> 100, East)
  val carry = new LogicProbe(300 -> 50, East, "Carry")
  val result = new LogicProbe(300 -> 150, East, "Result")


  val wires:Seq[Wire] = Seq(
    (a1.t -> adder.ta).wireVia(adder.ta.x -> a1.t.y),
    (b1.t -> adder.tb).wireVia(adder.tb.x -> b1.t.y),
    (adder.tc -> carry.t).wireVia(adder.tc.x -> carry.t.y),
    (adder.tr -> result.t).wireVia(adder.tr.x -> result.t.y),
  )

  val circuit = new Circuit(Seq(a1, b1, adder, result, carry) ++ wires, 600, 200)
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

  override protected def render: DiffNode[Element, Node] = <.div(
    Challenge.textAndEx(
      Common.marked(
        """
          |# Half Adder
          |
          |In an earlier topic, we showed you the circuit logic for a [half-adder](#/boolean/4/0). Go on, take a look to
          |remind yourself, then click the browser back button to come back here.
          |
          |Now that we've introduced boolean logic, we can start to abstract away from the gate circuit itself and
          |just talk about the logic it implements. So, on this page we've got the same exercise (filling in the
          |truth table by toggling the inputs), but we've replaced the details of the circuitry with a "black box" that
          |just shows its inputs and outputs and labels what it does.
          |
          |(Yes, the box looks white on the screen. It's called a "black box" because you can't see its internal
          |workings.)
          |""".stripMargin
      ),
    )(Challenge.textColumn(
      <.div(^.cls := "card",
        <.div(^.cls := "card-header", "Exercise"),
        <.div(^.cls := "card-body",
          circuit,
          Common.marked(
            s"""
              |Toggle the inputs on the half-adder to fill out the truth table below. We've used a "black box" version
              |of the half-adder, though we've shown the logic in the headings of the table.
              |
              |${TruthTable(Seq("A", "B"), Seq("Carry = A·B", "Result = A⊕B"), truthTable, a1.value.toSeq ++ b1.value.toSeq).htmlString}
              |""".stripMargin)
        )
      ),
      if (isComplete) <.div(
        Common.marked("After a while, you'll get to recognise some bit patterns (especially `#F` being `1111`)."),
        nextButton()
      ) else <.p()
    ))
  )

}
