package circuitsup.latches

import circuitsup.Common
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, ^}
import com.wbillingsley.veautiful.doctacular.Challenge
import com.wbillingsley.veautiful.doctacular.Challenge.{Complete, Open}
import com.wbillingsley.wren.Orientation.East
import com.wbillingsley.wren.Wire._
import com.wbillingsley.wren._
import org.scalajs.dom.{Element, Node}
import LatchesTopic.{nextButton, onCompletionUpdate}

object SRLatchLogic extends ExerciseStage {

  given wireCol:Wire.ColouringRule = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val s:LogicInput = new LogicInput(100 ->190, East, name="~S", initial=Some(true))({ v =>  onUpdate() })
  val r:LogicInput = new LogicInput(100 ->310, East, name="~R", initial=Some(true))({ v =>  onUpdate() })

  val nand1 = new NandGate(200 -> 200, East, Some("Nand1"))
  val nand2 = new NandGate(200 -> 300, East, Some("Nand2"))

  val q = new LogicProbe(300 -> 200, East, "Q")
  val qbar = new LogicProbe(300 -> 300, East, "~Q")

  val wires:Seq[Wire] = Seq(
    (s.t -> nand1.ta).wire,
    (r.t -> nand2.tb).wire,

    (nand1.out -> nand2.ta).wireVia(nand1.out.x -> (nand1.out.y + 10), nand2.ta.x -> (nand2.ta.y - 10)),
    (nand2.out -> nand1.tb).wireVia(nand2.out.x -> (nand2.out.y - 10), nand1.tb.x -> (nand1.tb.y + 10)),

    (nand1.out -> q.t).wire,
    (nand2.out -> qbar.t).wire,
  )

  val circuit = new Circuit(Seq(s, r, nand1, nand2, q, qbar) ++ wires, 600, 600)
  val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
  propagator.resolve()

  def checkCompletion:Boolean = buildStage == 5

  def stringify(o:Option[Boolean]):String = o match {
    case Some(true) => "1"
    case Some(false) => "0"
    case _ => "?"
  }

  def onUpdate():Unit = {
    propagator.resolve()

    for {
      ss <- s.value; rr <- r.value; qq <- q.value
    } {
      if (!ss && rr && qq & buildStage == 1) buildStage = 2
      if (ss && rr && qq & buildStage == 2) buildStage = 3
      if (ss && !rr && !qq & buildStage == 3) buildStage = 4
      if (ss && rr && !qq & buildStage == 4) buildStage = 5
    }

    if (checkCompletion) {
      completion = Complete(Some(1), None)
      onCompletionUpdate()
    } else {
      rerender()
    }
  }

  var buildStage = 1
  def steps = Seq(
    <.li(
      Common.marked(
      """
        |**Set `~S` low**. The output of the top NAND gate (`Q`) will then be `1`, *regardless of what the other input is*.
        |Remember, `~(0 && 0) = 1` and `~(0 && 1) = 1` so it doesn't matter what the gate 's second input is.
        |""".stripMargin),
      <.button(^.cls := "btn btn-outline-primary",
        ^.onClick --> {
          s.value = Some(false)
          r.value = Some(true)
          onUpdate()
        },
        "~S → 0; ~R → 1"
      )
    ),
    <.li(
      Common.marked(
        """
          |Now that `Q` and `~R` are both `1`, the bottom NAND gate sets `~Q` to `0`. This is fed back into the top
          |NAND gate. Now... **set `~S` back to `1`**.
          |""".stripMargin),
      <.button(^.cls := "btn btn-outline-primary",
        ^.onClick --> {
          s.value = Some(true)
          r.value = Some(true)
          onUpdate()
        },
        "~S → 1; ~R → 1"
      )
    ),
    <.li(
      Common.marked(
        """
          |`~Q` is `0` and is being fed into the top NAND gate. That keeps `Q` high, even though `~S` has gone high again.
          |We're in a stable output state with `Q`=`1` and `~Q`=`0`. Now, **set `~R` low**.
          |""".stripMargin),
      <.button(^.cls := "btn btn-outline-primary",
        ^.onClick --> {
          s.value = Some(true)
          r.value = Some(false)
          onUpdate()
        },
        "~S → 1; ~R → 0"
      )
    ),
    <.li(
      Common.marked(
        """
          |`~R` being `0` has driven `~Q` high. `~Q` and `~S` both being `1`, that sets `Q` to `0`. Now, **set `~R` high again**.
          |""".stripMargin),
      <.button(^.cls := "btn btn-outline-primary",
        ^.onClick --> {
          if (buildStage < 5) buildStage += 1;
          s.value = Some(true)
          r.value = Some(true)
          onUpdate()
        },
        "~S → 1; ~R → 1"
      )
    ),
    <.li(
      Common.marked(
        """
          |`Q` being low has kept `~Q` high. `~Q` and `~S` being high mean that `Q` stays low. We've reached another stable state,
          |with `Q`=`0` and `~Q`=`1`
          |""".stripMargin
      )
    )
  )

  override protected def render = <.div(Challenge.textAndEx(
    <.div(
      Common.marked(
        s"""
           |### SR Latch
           |
           |Let's start by introducing the Set-Reset (SR) Latch. This uses a feedback loop to preserve a little bit of
           |state.
           |
           |Our two inputs are `~S` (set) and `~R` (reset). I've put a `~` on the names because we will turn these
           |*low* to enact their operation. (In other notes, you might see them written <span style="text-decoration: overline;">S</span>
           |and <span style="text-decoration: overline;">R</span>, but we'll use `~` here.)
           |
           |I've set it up so `~S` and `~R` are both `1`. At the moment, the output is unknown. The next steps are
           |a little complex to explain, so each one has a button to do it for you.
           |
           |""".stripMargin
      ),
      <.ol(
        steps.take(buildStage)
      ),
      if (isComplete) <.div(
        Common.marked(
          s"""
             |If its inputs are both high, the SR latch can be in one of two stable states: `Q`=`1` or `Q`=`0`. This
             |*bistable* property means we can use it to store state.
             |""".stripMargin
        ), nextButton()
      )else <.div()
    )
  )(<.div(
    circuit
  )))

}
