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

object RippleCarryClocked extends ExerciseStage {

  given wireCol:Wire.ColouringRule = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val bits = 4

  val adders = for { i <- 0 until bits } yield new FullAdder(300 -> ((i * 130) + 80), East)

  val aBits = for {i <- 0 until bits} yield new LogicProbe(100 -> (adders(i).ta.y - 10), West, name=s"A${bits - i - 1}")
  val bBits = for {i <- 0 until bits} yield new LogicProbe(200 -> adders(i).tb.y, West, name=s"B${bits - i - 1}")

  val reg = for {i <- 0 until bits } yield new FlipFlop(450 -> (adders(i).tr.y + 20))
  val regj = for {i <- 0 until bits } yield new Junction(400 -> reg(i).clk.y)


  val oBits = for {i <- 0 until bits} yield new LogicProbe(520 -> reg(i).q.y, East, s"Bit ${bits - i - 1}")

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
  val clock = new LogicProbe(400 -> (adders.last.tcin.y + 50), West, "Clock")

  val wires:Seq[Wire] = Seq(
    (cin.t -> adders.last.tcin).wireVia(adders.last.tcin.x -> cin.t.y),
    (adders.head.tcout -> carry.t).wireVia(adders.head.tcout.x -> carry.t.y),
    (clock.t -> reg.last.clk).wireVia(clock.t.x -> reg.last.clk.y),
  ) ++ ((0 until bits).flatMap { i => Seq(
    (aBits(i).t -> adders(i).ta).wireVia(adders(i).ta.x -> aBits(i).t.y),
    (bBits(i).t -> adders(i).tb).wire,
    (adders(i).tr -> reg(i).td).wire,
    (reg(i).q -> oBits(i).t).wire,
    (regj(i).terminal -> reg(i).clk).wire
  ) }) ++ (for {
    (to, from) <- adders.zip(adders.tail)
  } yield (from.tcout -> to.tcin).wireVia(from.tcout.x -> (from.tcout.y - 80), to.tcin.x -> (from.tcout.y - 80))) ++ (
    for { (to, from) <- regj.zip(regj.tail) } yield (to.terminal -> from.terminal).wire
  )

  val circuit = new Circuit(aBits ++ bBits ++ adders ++ reg ++ regj ++ oBits ++ Seq(cin, carry, clock) ++ wires, 700, 600)
  val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
  propagator.resolve()

  val logicPlot = LogicPlot(adders.map(_.tr.potential) :+ clock.t.potential, width=200, names = adders.indices.map(x => x -> s"Bit ${3 - x}").toMap + (adders.length -> "Clock"))
  val logicPlot2 = LogicPlot(reg.map(_.q.potential) :+ clock.t.potential, width=200, names = adders.indices.map(x => x -> s"Bit ${3 - x}").toMap + (adders.length -> "Clock"))
  var lastLogic = 0d
  var lastClock = 0d
  val animation = AnimationPlayer(16) { d =>

    if (d - lastClock > 1000) {
      lastClock = d
      clock.t.potential.content = clock.t.potential.value match {
        case Some(x) => if (x > 3) Some(0d -> QuestionSet) else Some(5d -> QuestionSet)
        case _ => Some(0d -> QuestionSet)
      }
    }

    if (!isComplete && checkCompletion) {
      completion = Complete(Some(1), None)
      onCompletionUpdate()
    }

    propagator.resolve(wires.flatMap(_.constraints))
    if (d - lastLogic > 200 && propagator.canStep) {
      lastLogic = d
      propagator.step()
    }

    logicPlot.updateValues()
    logicPlot2.updateValues()
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
             |### Ripple Carry Adder, with clock
             |
             |Let's now go back to our ripple carry adder, and we'll put the outputs from the adder stages into the
             |edge-triggered flip flops of our "register". Run the simulation, and notice that though the logic levels
             |still take time to ripple through the adder, they are all clocked into the flip-flops at once.
             |
             |Below, we've charted the output of the adder stages, versus the output of the flip-flops.
             |
             |""".stripMargin
        ),
      ),
      animation,
      Challenge.split(
        <.div(^.cls := "card mr-2",
          <.div(^.cls := "card-header", "Unclocked"),
          <.div(^.cls := "card-body", logicPlot)
        )
      )(
        <.div(^.cls := "card",
          <.div(^.cls := "card-header", "Clocked"),
          <.div(^.cls := "card-body", logicPlot2)
        )
      ),
      <.p(
        """
          |Now, go and toggle the "carry in" on the circuit up and down, and see what happens to the chart, the outputs,
          |and even the "result" shown under the circuit.
          |""".stripMargin
      ),
      if (isComplete) <.div(
        <.div(Common.marked(
          """
            | Our clock runs fairly slowly, at 1 cycle per second (1 Hertz), but the clocks in many computers might run at 3GHz.
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
