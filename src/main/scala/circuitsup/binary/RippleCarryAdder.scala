package circuitsup.binary

import circuitsup.Common
import circuitsup.binary.BinaryTopic.{nextButton, onCompletionUpdate}
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, ^}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Open}
import com.wbillingsley.wren.Orientation.East
import com.wbillingsley.wren.Wire._
import com.wbillingsley.wren._
import org.scalajs.dom.{Element, Node}

object RippleCarryAdder extends ExerciseStage {

  implicit val wireCol = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val bits = 4

  val adders = for { i <- 0 until bits } yield new FullAdder(300 -> ((i * 150) + 80), East)

  val a = for { i <- 0 until bits } yield new LogicInput(100 -> (adders(i).ta.y - 10), East, name=s"A${bits - i - 1}")({ _ => onUpdate() })
  val b = for { i <- 0 until bits } yield new LogicInput(200 -> adders(i).tb.y, East, name=s"B${bits - i - 1}")({ _ => onUpdate() })
  val o = for { i <- 0 until bits } yield new LogicProbe(400 -> adders(i).tr.y, East, s"Bit ${bits - i - 1}")


  val cin = new LogicInput(150 -> (adders.last.tcin.y + 50), East, name=s"Carry in")({ _ => onUpdate() })
  val carry = new LogicProbe(400 -> 30, East, "Carry out")

  val wires:Seq[Wire] = Seq(
    (cin.t -> adders.last.tcin).wireVia(adders.last.tcin.x -> cin.t.y),
    (adders.head.tcout -> carry.t).wireVia(adders.head.tcout.x -> carry.t.y)
  ) ++ ((0 until bits).flatMap { i => Seq(
    (a(i).t -> adders(i).ta).wireVia(adders(i).ta.x -> a(i).t.y),
    (b(i).t -> adders(i).tb).wire,
    (adders(i).tr -> o(i).t).wire
  ) }) ++ (for {
    (to, from) <- adders.zip(adders.tail)
  } yield (from.tcout -> to.tcin).wireVia(from.tcout.x -> (from.tcout.y - 80), to.tcin.x -> (from.tcout.y - 80)))

  val circuit = new Circuit(a ++ b ++ adders ++ o ++ Seq(cin, carry) ++ wires, 600, 700)
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
          |# Ripple Carry Adder
          |
          |Full adders are used to add two bits of a binary number. To add numbers (made up of several bits) together, we're going to need
          |full adders for each bit.
          |
          |In the exercise box, we've connected a series of four Full Adders. Each bit has its "carry out" connected to
          |the next bit's "carry in". This lets the carry "ripple" up through the circuit as each bit is worked out.
          |
          |In the diagram, the "least significant bit" (the `1`s bit) is at the bottom of the circuit, and the "most
          |significant bit" (here, the `8`s column) is at the top.
          |
          |Use the adder (toggle the input bits) to work out the following sums:
          |
          |* `3` + `7` = ?
          |* `#A` + `4` = ?
          |* `#B` + `4` + `1` = ?
          |* `#F` + `0` + `1` = ?
          |
          |(Use the carry in for the last two)
          |""".stripMargin
      ),
    )(Challenge.textColumn(
      <.div(^.cls := "card",
        <.div(^.cls := "card-header", "Exercise"),
        <.div(^.cls := "card-body",
          circuit,
          Common.marked(
            s"""
              |Use the four-bit ripple carry adder above to add the numbers on the left.
              |""".stripMargin)
        )
      ),
      if (isComplete) <.div(
        Common.marked(
          """
            |In the last example, where we had one input set to #F (15), toggling the carry input to 1 would ripple
            |through all the adders, turning all their carry outputs to 1. This happens fairly quickly, but it does
            |take a little time for the voltages to propagate through the circuit.
            |""".stripMargin),
        nextButton()
      ) else <.p()
    ))
  )

}
