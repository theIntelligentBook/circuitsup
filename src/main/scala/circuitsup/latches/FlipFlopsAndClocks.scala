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

object FlipFlopsAndClocks extends ExerciseStage {

  given wireCol:Wire.ColouringRule = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val d:LogicInput = new LogicInput(100 ->80, East, name="D", initial=Some(false))({ v =>  onUpdate() })
  val e:LogicProbe = new LogicProbe(100 ->120, West, name="Clock")

  val flipflop = new FlipFlop(200 -> 100)

  val q = new LogicProbe(300 -> 80, East, "Q")
  val qbar = new LogicProbe(300 -> 120, East, "~Q")

  val wires:Seq[Wire] = Seq(
    (d.t -> flipflop.td).wire,
    (e.t -> flipflop.clk).wire,

    (flipflop.q -> q.t).wire,
    (flipflop.qbar -> qbar.t).wire,
  )

  val circuit = new Circuit(Seq(d, e, flipflop, q, qbar) ++ wires, 600, 200)
  val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
  propagator.resolve()

  val toPlot = Seq(d.t.potential, e.t.potential, q.t.potential)
  val logicPlot = LogicPlot(toPlot, Map(0 -> "D", 1 -> "E", 2 -> "Q"), width=700)

  var lastClock = 0d
  val animation = AnimationPlayer(16) { time =>

    propagator.resolve()

    if (time - lastClock > 1000) {
      lastClock = time
      e.t.potential.content = e.t.potential.value match {
        case Some(x) => if (x > 3) Some(0d -> QuestionSet) else Some(5d -> QuestionSet)
        case _ => Some(0d -> QuestionSet)
      }
    }

    for {
      qq <- q.value
    } {
      if (!qq && buildStage == 1) buildStage = 2
      if (qq && buildStage == 2) buildStage = 3
      if (!qq && buildStage == 3) buildStage = 4
    }

    if (checkCompletion) {
      completion = Complete(Some(1), None)
      onCompletionUpdate()
    } else {
      rerender()
    }

    logicPlot.updateValues()
    rerender()
  }
  def checkCompletion:Boolean = buildStage == 4

  def stringify(o:Option[Boolean]):String = o match {
    case Some(true) => "1"
    case Some(false) => "0"
    case _ => "?"
  }

  def onUpdate():Unit = {

  }

  var buildStage = 1
  def steps = Seq(
    <.li(
      Common.marked(
      """
        |**Start the simulation**, and notice that the `0` input to `D` flows through to the output of the flip-flop on
        |the rising edge of the clock.
        |""".stripMargin),
    ),
    <.li(
      Common.marked(
      """
        |Now **turn `D` high**. Again, no matter when during the **clock cycle** you changed `D`, it will flow through
        |to the output on the next rising edge of the clock.
        |""".stripMargin),
    ),
    <.li(
      Common.marked(
        """
          |And **turn `D` to `0`** again, and let that flow through with the clock.
          |""".stripMargin),
    ),
  )

  override protected def render: DiffNode[Element, Node] = <.div(Challenge.textAndEx(
    <.div(
      <.div(Common.marked(
        s"""
           |### Positive-edge triggered D Flip Flop, with clock
           |
           |Let's take the D-Flip Flop circuit, and put it in a component. The one we're showing on the right is
           |triggered on the *rising edge* of the `enable` input (just think of there being an extra `NOT` gate on that
           |signal).
           |
           |However, we've also done something else: we've taken control of the `enable` signal away from you and
           |instead we've set it to rise and fall at regular intervals. This is a **clock**. Most processors
           |use a clock to keep logic updates in sync throughout the circuit.
           |""".stripMargin
      )),
      <.ol(
        steps.take(buildStage)
      ),
      if (isComplete) <.div(
        Common.marked(
          s"""
             |Using a clock, and edge-triggered flip-flops, we now have a way that we can let logic circuits
             |(like our ripple carry adder) settle, and only read their outputs as inputs at known times.
             |""".stripMargin
        ), nextButton()
      )else <.div()
    )
  )(
    Challenge.textColumn(
      <.div(^.cls := "card",
        <.div(^.cls := "card-header", "Rising-edge triggered D Flip Flop"),
       <.div(^.cls := "card-body", circuit),
        <.div(^.cls := "card-body", "Press start to start the simulation and graph. ", animation), <.div(^.cls := "card-body", logicPlot)
      )
    )
  ))

}
