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

object GatedSRLatch extends ExerciseStage {

  implicit val wireCol = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val s:LogicInput = new LogicInput(100 ->190, East, name="S", initial=Some(false))({ v =>  onUpdate() })
  val r:LogicInput = new LogicInput(100 ->310, East, name="R", initial=Some(false))({ v =>  onUpdate() })
  val e:LogicInput = new LogicInput(100 ->250, East, name="E", initial=Some(false))({ v =>  onUpdate() })

  val nand1 = new NandGate(200 -> 200, East, Some("Nand1"))
  val nand2 = new NandGate(200 -> 300, East, Some("Nand2"))

  val nand3 = new NandGate(300 -> 210, East, Some("Nand1"))
  val nand4 = new NandGate(300 -> 290, East, Some("Nand2"))

  val q = new LogicProbe(400 -> 210, East, "Q")
  val qbar = new LogicProbe(400 -> 290, East, "~Q")

  val wires:Seq[Wire] = Seq(
    (s.t -> nand1.ta).wire,
    (r.t -> nand2.tb).wire,

    (e.t -> nand1.tb).wireVia(e.t.x -> nand1.tb.y),
    (e.t -> nand2.ta).wireVia(e.t.x -> nand2.ta.y),

    (nand1.out -> nand3.ta).wire,
    (nand2.out -> nand4.tb).wire,

    (nand3.out -> nand4.ta).wireVia(nand3.out.x -> (nand3.out.y + 10), nand3.ta.x -> (nand4.ta.y - 10)),
    (nand4.out -> nand3.tb).wireVia(nand4.out.x -> (nand4.out.y - 10), nand4.tb.x -> (nand3.tb.y + 10)),

    (nand3.out -> q.t).wire,
    (nand4.out -> qbar.t).wire,
  )

  val circuit = new Circuit(Seq(s, r, e, nand1, nand2, nand3, nand4, q, qbar) ++ wires, 600, 600)
  val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
  propagator.resolve()

  def checkCompletion:Boolean = buildStage == 6

  def stringify(o:Option[Boolean]):String = o match {
    case Some(true) => "1"
    case Some(false) => "0"
    case _ => "?"
  }

  def onUpdate():Unit = {
    propagator.resolve()

    for {
      ss <- s.value; rr <- r.value; ee <- e.value
    } {
      if (ss && !ee && !rr && buildStage == 1) buildStage = 2
      if (ss && ee && !rr && buildStage == 2) buildStage = 3
      if (ss && !ee && !rr && buildStage == 3) buildStage = 4
      if (!ss && !ee && rr && buildStage == 4) buildStage = 5
      if (!ss && ee && rr && buildStage == 5) buildStage = 6
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
        |`E` is low. **Set `S` to `1`** and notice it has no effect on `Q`. `E` being low will keep the top left NAND gate's output
        |high no matter what `S` is.
        |""".stripMargin),
      <.button(^.cls := "btn btn-outline-primary",
        ^.onClick --> {
          s.value = Some(true)
          r.value = Some(false)
          e.value = Some(false)
          onUpdate()
        },
        "S → 1; R → 0; E → 0"
      )
    ),
    <.li(
      Common.marked(
        """
          |Now **toggle `E` to `1`**. With the gate enabled, the `1` in `S` can flow through to our SR-Latch and it
          |should move into the `Q=1` state.
          |""".stripMargin),
      <.button(^.cls := "btn btn-outline-primary",
        ^.onClick --> {
          s.value = Some(true)
          r.value = Some(false)
          e.value = Some(true)
          onUpdate()
        },
        "S → 1; R → 0; E → 1"
      )
    ),
    <.li(
      Common.marked(
        """
          |Now, **set `E` low** to disable the gate again.
          |""".stripMargin),
      <.button(^.cls := "btn btn-outline-primary",
        ^.onClick --> {
          s.value = Some(true)
          r.value = Some(false)
          e.value = Some(false)
          onUpdate()
        },
        "S → 1; R → 0; E → 0"
      )
    ),
    <.li(
      Common.marked(
        """
          |With the gate disabled, we can change our inputs. **Set `S` to `0` and `R` to `1`**. It shouldn't have any
          |effect on `Q` yet.
          |""".stripMargin),
      <.button(^.cls := "btn btn-outline-primary",
        ^.onClick --> {
          s.value = Some(false)
          r.value = Some(true)
          e.value = Some(false)
          onUpdate()
        },
        "S → 0; R → 1; E → 0"
      )
    ),
    <.li(
      Common.marked(
        """
          |Now, **set `E` to `1` to let the inputs have effect**.
          |""".stripMargin),
      <.button(^.cls := "btn btn-outline-primary",
        ^.onClick --> {
          s.value = Some(false)
          r.value = Some(true)
          e.value = Some(true)
          onUpdate()
        },
        "S → 0; R → 1; E → 0"
      )
    ),
    <.li(
      Common.marked(
        """
          |With the gate enabled, the reset signal flows through, and the SR-Latch should move to the `Q=0` state.
          |""".stripMargin
      )
    )
  )

  override protected def render: DiffNode[Element, Node] = <.div(Challenge.textAndEx(
    <.div(
      Common.marked(
        s"""
           |### Gated SR Latch
           |
           |On the right hand side, we've got an SR Latch, but to the left of it we've put two more NAND gates.
           |There are now three inputs: `S` and `R` (not inverted), and `E` (enable).
           |
           |What we'll find is that setting `S` to `1` or `R` to `1` work as "set" or "reset" inputs, but only when
           |`E` is high. If `E` is low, the outputs of the NAND gates on the left (the inputs to the SR Latch) will
           |both be `1` and the SR-Latch will remain in whatever stable state it's in.
           |""".stripMargin
      ),
      <.ol(
        steps.take(buildStage)
      ),
      if (isComplete) <.div(
        Common.marked(
          s"""
             |We've now got a device that has inputs (`S` and `R`) but also a control input (`E`) for when the inputs
             |will be "read" by the SR-Latch.
             |""".stripMargin
        ), nextButton()
      )else <.div()
    )
  )(<.div(
    circuit
  )))

}
