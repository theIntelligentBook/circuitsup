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

object DLatch extends ExerciseStage {

  implicit val wireCol = Wire.voltageColoring

  var completion: Challenge.Completion = Open

  val d:LogicInput = new LogicInput(100 ->190, East, name="D", initial=Some(false))({ v =>  onUpdate() })
  val e:LogicInput = new LogicInput(170 ->250, East, name="E", initial=Some(false))({ v =>  onUpdate() })

  val nand1 = new NandGate(200 -> 200, East, Some("Nand1"))
  val nand2 = new NandGate(200 -> 300, East, Some("Nand2"))

  val nand3 = new NandGate(300 -> 210, East, Some("Nand3"))
  val nand4 = new NandGate(300 -> 290, East, Some("Nand4"))

  val not = new NotGate(130 -> 310)

  val q = new LogicProbe(400 -> 210, East, "Q")
  val qbar = new LogicProbe(400 -> 290, East, "~Q")

  val wires:Seq[Wire] = Seq(
    (d.t -> nand1.ta).wire,
    (d.t -> not.in).wire,
    (not.out -> nand2.tb).wire,

    (e.t -> nand1.tb).wireVia(e.t.x -> nand1.tb.y),
    (e.t -> nand2.ta).wireVia(e.t.x -> nand2.ta.y),

    (nand1.out -> nand3.ta).wire,
    (nand2.out -> nand4.tb).wire,

    (nand3.out -> nand4.ta).wireVia(nand3.out.x -> (nand3.out.y + 10), nand3.ta.x -> (nand4.ta.y - 10)),
    (nand4.out -> nand3.tb).wireVia(nand4.out.x -> (nand4.out.y - 10), nand4.tb.x -> (nand3.tb.y + 10)),

    (nand3.out -> q.t).wire,
    (nand4.out -> qbar.t).wire,
  )

  val circuit = new Circuit(Seq(d, e, nand1, nand2, nand3, nand4, not, q, qbar) ++ wires, 600, 600)
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
      ss <- d.value; ee <- e.value
    } {
      if (ss && !ee && buildStage == 1) buildStage = 2
      if (ss && ee  && buildStage == 2) buildStage = 3
      if (ss && !ee  && buildStage == 3) buildStage = 4
      if (!ss && !ee && buildStage == 4) buildStage = 5
      if (!ss && ee && buildStage == 5) buildStage = 6
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
        |`E` is low. **Set `D` to `1`** and notice it has no effect on `Q`. `E` being low will keep the top left NAND gate's output
        |high no matter what `S` is.
        |""".stripMargin),
      <.button(^.cls := "btn btn-outline-primary",
        ^.onClick --> {
          d.value = Some(true)
          e.value = Some(false)
          onUpdate()
        },
        "D → 1; E → 0"
      )
    ),
    <.li(
      Common.marked(
        """
          |Now **toggle `E` to `1`**. The latch turns transparent and the `1` in `D` can flow through to `Q`.
          |""".stripMargin),
      <.button(^.cls := "btn btn-outline-primary",
        ^.onClick --> {
          d.value = Some(true)
          e.value = Some(true)
          onUpdate()
        },
        "D → 1; E → 1"
      )
    ),
    <.li(
      Common.marked(
        """
          |Now, **set `E` low** to disable the gate again.
          |""".stripMargin),
      <.button(^.cls := "btn btn-outline-primary",
        ^.onClick --> {
          d.value = Some(true)
          e.value = Some(false)
          onUpdate()
        },
        "D → 1; E → 0"
      )
    ),
    <.li(
      Common.marked(
        """
          |With the gate disabled, we can change our inputs. **Set `D` to `0`**. It shouldn't have any
          |effect on `Q` yet.
          |""".stripMargin),
      <.button(^.cls := "btn btn-outline-primary",
        ^.onClick --> {
          d.value = Some(false)
          e.value = Some(false)
          onUpdate()
        },
        "D → 0; E → 0"
      )
    ),
    <.li(
      Common.marked(
        """
          |Now, **set `E` to `1` to let the input have effect**.
          |""".stripMargin),
      <.button(^.cls := "btn btn-outline-primary",
        ^.onClick --> {
          d.value = Some(false)
          e.value = Some(true)
          onUpdate()
        },
        "D → 0; E → 1"
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
           |### D Latch
           |
           |On the right hand side, we've taken a Gated SR-Latch, and we've added a NOT gate tying two of the inputs
           |together. This produces a *D Latch* or *transparent latch*.
           |
           |Whenever the `E` input is high, the latch will be "transparent" (changes to the input, now called `D`, will
           |flow through to the output). Whenever the `E` input is low, changes to `D` will have no effect - the device
           |is *latched*.
           |""".stripMargin
      ),
      <.ol(
        steps.take(buildStage)
      ),
      if (isComplete) <.div(
        Common.marked(
          s"""
             |We've now got a device that has one input (`D`) and a control input (`E`) for when it can take effect.
             |""".stripMargin
        ), nextButton()
      )else <.div()
    )
  )(<.div(
    circuit
  )))

}
