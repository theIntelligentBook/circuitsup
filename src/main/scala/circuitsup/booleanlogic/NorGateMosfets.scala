package circuitsup.booleanlogic

import circuitsup.Common
import circuitsup.booleanlogic.BooleanTopic.{nextButton, onCompletionUpdate}
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.<
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Open}
import com.wbillingsley.wren.Orientation.{East, North}
import com.wbillingsley.wren.Wire._
import com.wbillingsley.wren._
import org.scalajs.dom.{Element, Node}

object NorGateMosfets extends ExerciseStage {

  implicit val wireCol = Wire.voltageColoring
  implicit val nMosCol = NMOSSwitch.voltageColouring
  implicit val pMosCol = PMOSSwitch.voltageColouring

  var completion: Challenge.Completion = Open

  val vdd = new VoltageSource(50 ->100, North, Some(5d))
  val gnd = new Ground(50, 350)

  val a1:LogicInput = new LogicInput(300 ->100, East, name="A")({ v => a2.value = v; onUpdate() })
  val b1:LogicInput = new LogicInput(300 ->200, East, name="B")({ v => b2.value = v; onUpdate() })
  val a2:LogicInput = new LogicInput(150 ->300, East, name="A")({ v => a1.value = v; onUpdate() })
  val b2:LogicInput = new LogicInput(300 ->300, East, name="B")({ v => b1.value = v; onUpdate() })

  val nmos1 = new PMOSSwitch(350 -> 100)
  val nmos2 = new PMOSSwitch(350 -> 200)

  val pmos1 = new NMOSSwitch(200 -> 300)
  val pmos2 = new NMOSSwitch(350 -> 300)

  val out = new LogicProbe(400 -> nmos2.drain.y, East)

  val wires:Seq[Wire] = Seq(
    (a1.t -> nmos1.gate).wire,
    (b1.t -> nmos2.gate).wire,
    (a2.t -> pmos1.gate).wire,
    (b2.t -> pmos2.gate).wire,
    (gnd.terminal -> pmos1.source).wireVia(pmos1.source.x -> gnd.terminal.y),
    (gnd.terminal -> pmos2.source).wireVia(pmos2.source.x -> gnd.terminal.y),
    (gnd.terminal -> vdd.t1).wire, (vdd.t2 -> nmos1.source).wireVia(vdd.t2.x -> nmos1.source.y),

    (nmos1.drain -> nmos2.source).wire, (nmos2.drain -> pmos2.drain).wire,
    (pmos1.drain -> nmos2.drain).wireVia(pmos1.drain.x -> nmos2.drain.y),
    (nmos2.drain -> out.t).wire
  )

  val labels = Seq(
    new ValueLabel("V" -> "DD", vdd.voltage, (vdd.x + 30) -> vdd.y),
  )

  val circuit = new Circuit(Seq(vdd, a1, b1, a2, b2, pmos1, pmos2, nmos1, nmos2, out, gnd) ++ wires ++ labels, 600, 600)
  val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
  propagator.resolve()

  def checkCompletion:Boolean = truthTable.size >= 4

  var truthTable = Map.empty[Seq[Boolean], Boolean]

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
      out <- out.value
    } truthTable = truthTable.updated(Seq(a, b), out)

    if (checkCompletion) {
      completion = Complete(Some(1), None)
      onCompletionUpdate()
    } else {
      rerender()
    }
  }

  override protected def render: DiffNode[Element, Node] = <.div(Challenge.textAndEx(
    <.div(
      Common.marked(
        s"""
           |### NOR gate in CMOS
           |
           |Rather than use a NOT gate and an OR gate, we can design a NOR gate directly using MOSFETs. Once again,
           |De Morgan's Laws help us do this simply.
           |
           |If `~(A+B) = (A'B')`, then we just need to take an AND gate and swap NMOS for PMOS and PMOS for NMOS.
           |
           |Go ahead - click back to the [AND gate in MOSFETs](#/boolean/0/0) and watch the negation circles on the
           |MOSFETs swap. Then click your browser back button to come back here.
           |
           |${TruthTable(Seq("A", "B"), truthTable, a1.value.toSeq ++ b1.value.toSeq).htmlString}
           |
           |It's time to click the logic input buttons to toggle them!
           |""".stripMargin
      ), if (isComplete) <.div(
        Common.marked(
          s"""
             | This has given us a NOR gate with only four MOSFETs instead of six.
             |""".stripMargin
        ), nextButton()
      )else <.div()
    )
  )(<.div(
    circuit
  )))

}
