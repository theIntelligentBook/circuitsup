package circuitsup.binary

import circuitsup.Common
import circuitsup.binary.BinaryTopic.{nextButton, onCompletionUpdate}
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, ^}
import com.wbillingsley.veautiful.doctacular.Challenge
import com.wbillingsley.veautiful.doctacular.Challenge.{Complete, Open}
import com.wbillingsley.wren.Orientation.East
import com.wbillingsley.wren.Wire._
import com.wbillingsley.wren._
import org.scalajs.dom.{Element, Node}

object FullAdderBox extends ExerciseStage {

  given wireCol:ColouringRule = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val c:LogicInput = new LogicInput(100 ->150, East, name="Cin")({ _ => onUpdate() })
  val a1:LogicInput = new LogicInput(100 ->50, East, name="A")({ _ => onUpdate() })
  val b1:LogicInput = new LogicInput(100 ->100, East, name="B")({ _ => onUpdate() })

  val adder = new FullAdder(200 -> 100, East)
  val carry = new LogicProbe(300 -> 50, East, "Carry")
  val result = new LogicProbe(300 -> 150, East, "Result")


  val wires:Seq[Wire] = Seq(
    (a1.t -> adder.ta).wireVia(adder.ta.x -> a1.t.y),
    (b1.t -> adder.tb).wireVia(b1.t.x -> adder.tb.y),
    (c.t -> adder.tcin).wireVia(adder.tcin.x -> c.t.y),
    (adder.tcout -> carry.t).wireVia(adder.tcout.x -> carry.t.y),
    (adder.tr -> result.t).wireVia(adder.tr.x -> result.t.y),
  )

  val circuit = new Circuit(Seq(c, a1, b1, adder, result, carry) ++ wires, 600, 200)
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
      cin <- c.value
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

  override protected def render = <.div(
    Challenge.textAndEx(
      Common.marked(
        """
          |# Full Adder
          |
          |In another previous exercise, we showed you the circuit logic for a [full-adder](#/boolean/4/1). Go on, take a look to
          |remind yourself, then click the browser back button to come back here.
          |
          |Again, let's replace our circuit with a "black box" depiction of the full adder, and ask you to toggle the
          |inputs to complete the truth table. You have three inputs now, because there's a carry input to the adder.
          |""".stripMargin
      ),
    )(Challenge.textColumn(
      <.div(^.cls := "card",
        <.div(^.cls := "card-header", "Exercise"),
        <.div(^.cls := "card-body",
          circuit,
          Common.marked(
            s"""
              |Toggle the inputs on the half-adder to fill out the truth table below. See if you can spot which
              |logic equation is the Result and which is the Carry.
              |
              |${TruthTable(Seq("A", "B", "Cin"), Seq("AB+Cin·(A+B)", "A⊕B⊕C"), truthTable, a1.value.toSeq ++ b1.value.toSeq ++ c.value.toSeq).htmlString}
              |""".stripMargin)
        )
      ),
      if (isComplete) <.div(
        Common.marked("Now that we can add individual bits, we can start thinking how to add numbers."),
        nextButton()
      ) else <.p()
    ))
  )

}
