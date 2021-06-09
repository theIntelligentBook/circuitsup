package circuitsup.latches

import circuitsup.Common
import circuitsup.latches.LatchesTopic.{nextButton, onCompletionUpdate}
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, ^}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Open}
import com.wbillingsley.wren.Orientation.{East, West}
import com.wbillingsley.wren.Wire._
import com.wbillingsley.wren._
import org.scalajs.dom.{Element, Node}

object RippleCarryTiming extends ExerciseStage {

  given wireCol:Wire.ColouringRule = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val bits = 4

  val adders = for { i <- 0 until bits } yield new FullAdder(300 -> ((i * 130) + 80), East)

  val aBits = for {i <- 0 until bits} yield new LogicProbe(100 -> (adders(i).ta.y - 10), West, name=s"A${bits - i - 1}")
  val bBits = for {i <- 0 until bits} yield new LogicProbe(200 -> adders(i).tb.y, West, name=s"B${bits - i - 1}")
  val oBits = for {i <- 0 until bits} yield new LogicProbe(400 -> adders(i).tr.y, East, s"Bit ${bits - i - 1}")

  // Set A to #f and B to #0
  for { bit <- aBits } bit.t.potential.content = Some(5d -> UserSet)
  for { bit <- bBits } bit.t.potential.content = Some(0d -> UserSet)

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

  val logicPlot = LogicPlot(adders.map(_.tr.potential), names = adders.indices.map(x => x -> s"Bit ${3 - x}").toMap)
  var lastLogic = 0d
  val animation = AnimationPlayer(16) { d =>
    if (!isComplete && checkCompletion) {
      completion = Complete(Some(1), None)
      onCompletionUpdate()
    }

    if (d - lastLogic > 160 && propagator.canStep) {
      lastLogic = d
      propagator.step()
    }

    logicPlot.updateValues()
    rerender()
  }

  def checkCompletion:Boolean = carry.t.potential.value.contains(5d)

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
      rerender()
  }

  override protected def render: DiffNode[Element, Node] = <.div(
    Challenge.textAndEx(
      <.div(
        Common.marked(
          s"""
             |### Ripple Carry Timing
             |
             |Electronics is fast, but it's not instant. Let's set up the Ripple Carry Adder that we saw previously again,
             |only this time let's fix all its inputs except the carry, so we can watch what happens as the carry ripples
             |through. I've also artificially slowed down the logic resolution, to make it more visible.
             |
             |This start/stop button will start and stop the animation (including the operation of the circuit). When
             |you're ready, start the animation and notice the logic lines in the graph (which will start in the
             |muddy middle)
             |
             |""".stripMargin
        ),
      ),
      animation,
      logicPlot,
      <.p(
        """
          |Now, go and toggle the "carry in" on the circuit up and down, and see what happens to the chart, the outputs,
          |and even the "result" shown under the circuit.
          |""".stripMargin
      ),
      if (isComplete) <.div(
        <.div(Common.marked(
          """
            |
            |While the carry was rippling through, we saw moments in time when the outputs were *wrong* because the
            |carry had not yet rippled all the way through the adder.
            |
            |The *static discipline* is that we want our inputs to be logic `0` and logic `1`, not in the ambiguous
            |in-between zone. The *dynamic discipline* is that we want our outputs to have settled to valid values
            |before they are read as inputs.
            |
            |This means we are going to need to introduce a timing signal that says *when* we can accept inputs:
            |a *clock*.
            |""".stripMargin)),
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
