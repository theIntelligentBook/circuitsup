package circuitsup.latches

import circuitsup.Common
import circuitsup.latches.LatchesTopic.{nextButton, onCompletionUpdate}
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, ^}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Open}
import com.wbillingsley.wren.Orientation.East
import com.wbillingsley.wren.Wire._
import com.wbillingsley.wren._
import org.scalajs.dom.{Element, Node}

object DFlipFlop extends ExerciseStage {

  given wireCol:Wire.ColouringRule = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val d:LogicInput = new LogicInput(100 ->190, East, name="D", initial=Some(false))({ v =>  onUpdate() })
  val e:LogicInput = new LogicInput(170 ->400, East, name="E", initial=Some(false))({ v =>  onUpdate() })

  val nand1 = new NandGate(210 -> 200, East, Some("Nand1"))
  val nand2 = new NandGate(210 -> 300, East, Some("Nand2"))
  val nand3 = new NandGate(300 -> 210, East, Some("Nand3"))
  val nand4 = new NandGate(300 -> 290, East, Some("Nand4"))

  val nand5 = new NandGate(410 -> 200, East, Some("Nand1"))
  val nand6 = new NandGate(410 -> 300, East, Some("Nand2"))
  val nand7 = new NandGate(500 -> 210, East, Some("Nand3"))
  val nand8 = new NandGate(500 -> 290, East, Some("Nand4"))

  val not = new NotGate(130 -> 310)
  val not2 = new NotGate(300 -> 400)

  val q = new LogicProbe(600 -> 210, East, "Q")
  val qbar = new LogicProbe(600 -> 290, East, "~Q")

  val wires:Seq[Wire] = Seq(
    (d.t -> nand1.ta).wire,
    (d.t -> not.in).wire,
    (not.out -> nand2.tb).wire,

    (e.t -> nand2.ta).wireVia(e.t.x -> nand2.ta.y),
    (nand1.tb -> nand2.ta).wire,
    (e.t -> not2.in).wire,
    (not2.out -> nand6.ta).wireVia((nand6.ta.x - 10) -> not2.out.y, (nand6.ta.x - 10) -> nand6.ta.y),
    (nand5.tb -> nand6.ta).wire,

    (nand1.out -> nand3.ta).wire,
    (nand2.out -> nand4.tb).wire,
    (nand5.out -> nand7.ta).wire,
    (nand6.out -> nand8.tb).wire,


    (nand3.out -> nand4.ta).wireVia(nand3.out.x -> (nand3.out.y + 10), nand3.ta.x -> (nand4.ta.y - 10)),
    (nand4.out -> nand3.tb).wireVia(nand4.out.x -> (nand4.out.y - 10), nand4.tb.x -> (nand3.tb.y + 10)),
    (nand7.out -> nand8.ta).wireVia(nand7.out.x -> (nand7.out.y + 10), nand7.ta.x -> (nand8.ta.y - 10)),
    (nand8.out -> nand7.tb).wireVia(nand8.out.x -> (nand8.out.y - 10), nand8.tb.x -> (nand7.tb.y + 10)),

    (nand3.out -> nand5.ta).wireVia(nand3.out.x -> nand5.ta.y),
    (nand4.out -> nand6.tb).wireVia(nand4.out.x -> nand6.tb.y),

    (nand7.out -> q.t).wire,
    (nand8.out -> qbar.t).wire,
  )

  val circuit = new Circuit(Seq(d, e, nand1, nand2, nand3, nand4, nand5, nand6, nand7, nand8, not, not2, q, qbar) ++ wires, 800, 450)
  val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
  propagator.resolve()

  val toPlot = Seq(d.t.potential, e.t.potential, q.t.potential)
  val logicPlot = LogicPlot(toPlot, Map(0 -> "D", 1 -> "E", 2 -> "Q"), width=700)
  val animation = AnimationPlayer(16) { time =>

    nand5.ta.potential.content = None
    nand6.tb.potential.content = None
    propagator.resolve()

    // Freshen slave inputs
    // TODO: fix this hack! When the master flip-flop is opaque, its output is considered "stale" and isn't used for updating the slave
    for {
      qq <- nand3.out.potential.value
      qd <- nand4.out.potential.value
    } {
      nand5.ta.potential.content = Some(qq -> QuestionSet)
      nand6.tb.potential.content = Some(qd -> QuestionSet)
    }
    propagator.resolve()


    for {
      dd <- d.value; qq <- q.value
    } {
      if (!qq && buildStage == 1) buildStage = 2
      if (dd && !qq && buildStage == 2) buildStage = 3
      if (qq && buildStage == 3) buildStage = 4
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
        |`D` is low. Clock it through to the `Q` output. You'll need to **set `E` to `1`, then set `E` back to `0` again**.
        |""".stripMargin),
    ),
    <.li(
      Common.marked(
      """
        |Now **turn `D` high**. For the moment, toggling `D` has no effect. Leave `D` high.
        |""".stripMargin),
    ),
    <.li(
      Common.marked(
        """
          |Now, **raise and lower `E` again**. Notice that this clocks the input through to `Q`. Also notice in the
          |diagram that the output always updates on the *falling edge* of `E`.
          |""".stripMargin),
    ),
  )

  override protected def render: DiffNode[Element, Node] = <.div(Challenge.textAndEx(
    <.div(
      <.div(Common.marked(
        s"""
           |### D Flip Flop
           |
           |D Latches are useful, but so long as the `E` signal is high, the output is transparent (and subject to
           |the input changing multiple times). If we connect two of them together, however, we can get a device that
           |appears to trigger on the *edge* of the `E` signal: a D *Flip Flop*.
           |
           |We have two latches connected together. The left one is called the "master" and the right one is called the
           |"slave". Whenever `E` is high, inputs can flow into the master latch. But the `E` input to the slave is
           |inverted - if `E` is high, the slave latch is opaque and the input won't go all the way through to the output.
           |
           |When `E` goes low, the master latch goes opaque, but the slave latch goes transparent - it takes whatever
           |value was in the master latch on to the output.
           |
           |Set the animation going (press play), and then we're going to get you to change the inputs.
           |""".stripMargin
      )),
      <.ol(
        steps.take(buildStage)
      ),
      if (isComplete) <.div(
        Common.marked(
          s"""
             |We now have an *edge-triggered* device. So long as our inputs are ready by the *falling edge* of `E`,
             |they'll be read and reflected at the output.
             |""".stripMargin
        ), nextButton()
      )else <.div()
    )
  )(
    <.div(^.cls := "card",
     <.div(^.cls := "card-body", circuit), <.div(^.cls := "card-body", animation), <.div(^.cls := "card-body", logicPlot)
    )
  ))

}
