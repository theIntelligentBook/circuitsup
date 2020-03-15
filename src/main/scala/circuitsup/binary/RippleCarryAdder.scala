package circuitsup.binary

import circuitsup.Common
import circuitsup.binary.BinaryTopic.{nextButton, onCompletionUpdate}
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, VHtmlNode, ^}
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

  val adders = for { i <- 0 until bits } yield new FullAdder(300 -> ((i * 130) + 80), East)

  val aBits = for {i <- 0 until bits} yield new LogicInput(100 -> (adders(i).ta.y - 10), East, name=s"A${bits - i - 1}")({ _ => onUpdate() })
  val bBits = for {i <- 0 until bits} yield new LogicInput(200 -> adders(i).tb.y, East, name=s"B${bits - i - 1}")({ _ => onUpdate() })
  val oBits = for {i <- 0 until bits} yield new LogicProbe(400 -> adders(i).tr.y, East, s"Bit ${bits - i - 1}")

  def a = Binary.sumBits(aBits.map(_.value))
  def b = Binary.sumBits(bBits.map(_.value))
  def ci = Binary.sumBits(Seq(cin.value))
  def o = Binary.sumBits(oBits.map(_.value))
  def co = Binary.sumBits(Seq(carry.value))

  val cin = new LogicInput(150 -> (adders.last.tcin.y + 50), East, name=s"Carry in")({ _ => onUpdate() })
  val carry = new LogicProbe(400 -> 30, East, "Carry out")

  val wires:Seq[Wire] = Seq(
    (cin.t -> adders.last.tcin).wireVia(adders.last.tcin.x -> cin.t.y),
    (adders.head.tcout -> carry.t).wireVia(adders.head.tcout.x -> carry.t.y)
  ) ++ ((0 until bits).flatMap { i => Seq(
    (aBits(i).t -> adders(i).ta).wireVia(adders(i).ta.x -> aBits(i).t.y),
    (bBits(i).t -> adders(i).tb).wire,
    (adders(i).tr -> oBits(i).t).wire
  ) }) ++ (for {
    (to, from) <- adders.zip(adders.tail)
  } yield (from.tcout -> to.tcin).wireVia(from.tcout.x -> (from.tcout.y - 80), to.tcin.x -> (from.tcout.y - 80)))

  val circuit = new Circuit(aBits ++ bBits ++ adders ++ oBits ++ Seq(cin, carry) ++ wires, 600, 600)
  val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
  propagator.resolve()

  def checkCompletion:Boolean = sumResults.forall(_._4.nonEmpty)

  val sumResults:Array[(Int, Int, Int, Option[(Int, Int)])] = Array(
    (3, 7, 0, None),
    (10, 4, 0, None),
    (11, 4, 1, None),
    (15, 0, 1, None)
  )

  def stringify(o:Option[Boolean]):String = o match {
    case Some(true) => "1"
    case Some(false) => "0"
    case _ => "?"
  }

  def stringifyCarry(o:Option[(Int, Int)]):String = o match {
    case Some((x, y)) if y > 0 => s"$x with overflow $y"
    case Some((x, _)) => x.toString
    case _ => "?"
  }

  def onUpdate():Unit = {
    propagator.clearCalculations()
    propagator.resolve()

    for {
      aa <- a
      bb <- b
      rr <- o
      cci <- ci
      cco <- co
    } {
      for {
        i <- sumResults.indices
        (qa, qb, qc, _) = sumResults(i)
      } {
        if (aa == qa && bb == qb && cci == qc) {
          sumResults(i) = (qa, qb, qc, Some(rr -> cco))
        }
      }
    }


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
        s"""
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
          |Use the adder (toggle the input bits) to work out the following sums, in the form A + B + C<sub>in</sub>:
          |${(for { (a, b, c, r) <- sumResults.toSeq } yield {
            s"* `$a` + `$b` + `$c` = ${stringifyCarry(r)}"
          }).mkString("\n")}
          |""".stripMargin
      ),
      if (isComplete) <.div(
        Common.marked(
          """
            |In the last example, where we had one input set to #F (15), toggling the carry input to 1 would ripple
            |through all the adders, turning all their carry outputs to 1. This happens fairly quickly, but it does
            |take a little time for the voltages to propagate through the circuit.
            |
            |You'll also notice that `15 + 1 = 0`. We've run out of bits and the addition has *overflowed* the size of
            |a 4-bit nibble. When you did the sum, the carry out from the ripple carry adder would have been `1`
            |
            |In computers, different integer representations can store different sizes of number.
            |
            |* An "unsigned byte" (8 bits) can store up to `255`
            |* An unsigned 16-bit number (often called a "short") can store up to `32,767`
            |* An unsigned 32-bit number (often called an "int") can store up to `2,147,483,647`
            |* An unsigned 64-bit number can store up to `18,446,744,073,709,551,615`
            |
            |""".stripMargin),
        nextButton()
      ) else <.p()
    )(Challenge.textColumn(
      <.div(^.cls := "card",
        <.div(^.cls := "card-body",
          <.p(circuit),
          <.p(^.cls := "row",
            <.div(^.cls := "col", "A: ", Binary.unsignedBinOpt(aBits.map(_.value))),
            <.div(^.cls := "col", "B: ", Binary.unsignedBinOpt(bBits.map(_.value)))
          ),
          <.p(^.cls := "row",
            <.div(^.cls := "col-2", "C", <("sub")("in"), ": ", Binary.unsignedBinOpt(Seq(cin.value), showHex=false, showDecimal=false)),
            <.div(^.cls := "col-8", "Result: ", Binary.unsignedBinOpt(oBits.map(_.value))),
            <.div(^.cls := "col-2", "C", <("sub")("out"), ": ", Binary.unsignedBinOpt(Seq(carry.value), showHex=false, showDecimal=false)),
          )
        )
      ),
    ))
  )

}
