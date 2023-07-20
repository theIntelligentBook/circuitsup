package circuitsup.booleanlogic

import circuitsup.templates.ExerciseStage
import BooleanTopic.nextButton
import BooleanTopic.onCompletionUpdate
import circuitsup.Common
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.<
import com.wbillingsley.veautiful.doctacular.Challenge
import com.wbillingsley.veautiful.doctacular.Challenge.{Complete, Open}
import com.wbillingsley.wren.Orientation.{East, North, South}
import com.wbillingsley.wren.{Circuit, ConstraintPropagator, Ground, LogicInput, LogicProbe, NMOSSwitch, PMOSSwitch, Terminal, TruthTable, ValueLabel, VoltageSource, Wire}
import org.scalajs.dom.{Element, Node}

import Wire._

object OrGateMosfets extends ExerciseStage {

  given wireCol:Wire.ColouringRule = Wire.voltageColoring
  given nMosCol:NMOSSwitch.ColouringRule = NMOSSwitch.voltageColouring
  given pMosCol:PMOSSwitch.ColouringRule = PMOSSwitch.voltageColouring

  var completion: Challenge.Completion = Open

  val vdd = new VoltageSource(50 ->200, North, Some(5d))
  val gnd = new Ground(50, 350)

  val a1:LogicInput = new LogicInput(150 ->100, East, name="A")({ v => a2.value = v; onUpdate() })
  val b1:LogicInput = new LogicInput(300 ->100, East, name="B")({ v => b2.value = v; onUpdate() })
  val a2:LogicInput = new LogicInput(300 ->200, East, name="A")({ v => a1.value = v; onUpdate() })
  val b2:LogicInput = new LogicInput(300 ->300, East, name="B")({ v => b1.value = v; onUpdate() })

  val nmos1 = new NMOSSwitch(200 -> 100)
  val nmos2 = new NMOSSwitch(350 -> 100)

  val pmos1 = new PMOSSwitch(350 -> 200)
  val pmos2 = new PMOSSwitch(350 -> 300)

  val out = new LogicProbe(400 -> pmos1.source.y, East)

  val wires:Seq[Wire] = Seq(
    (a1.t -> nmos1.gate).wire,
    (b1.t -> nmos2.gate).wire,
    (a2.t -> pmos1.gate).wire,
    (b2.t -> pmos2.gate).wire,

    (gnd.terminal -> pmos2.drain).wireVia(pmos2.drain.x -> gnd.terminal.y),
    (pmos2.source -> pmos1.drain).wire,
    (gnd.terminal -> vdd.t1).wire, (vdd.t2 -> nmos1.drain).wireVia(vdd.t2.x -> nmos1.drain.y),
    (nmos1.drain -> nmos2.drain).wire,
    (nmos2.source -> pmos1.source).wire,
    (nmos1.source -> pmos1.source).wireVia(nmos1.source.x -> pmos1.source.y),
    (pmos1.source -> out.t).wire
  )

  val labels = Seq(
    new ValueLabel("V" -> "DD", vdd.voltage, (vdd.x + 30) -> vdd.y),
  )

  val circuit = new Circuit(Seq(vdd, a1, b1, a2, b2, pmos1, pmos2, nmos1, nmos2, out, gnd) ++ wires ++ labels, 600, 600)
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

    for {
      a <- a1.value
      b <- b1.value
      out <- out.value
    } truthTable = truthTable.updated(Seq(a, b), Seq(out))

    if (checkCompletion) {
      completion = Complete(Some(1), None)
      onCompletionUpdate()
    } else {
      rerender()
    }
  }

  override protected def render = {
    import Common.notS

    <.div(Challenge.textAndEx(
      <.div(
        Common.marked(
          s"""
             |### OR gate in CMOS
             |
             |This time, let's show a MOSFET circuit for an OR gate, and toggle it to fill in the truth table.
             |
             |You'll notice that now A and B are connected to V<sub>DD</sub> in parallel, so if *either* of them is 1
             |there is a connection high.
             |
             |Complementing them, ${notS{"A"}} and ${notS("B")} are connected to ground in series, so *both* A and B must
             |be low to make a connection low.
             |
             |${TruthTable(Seq("A", "B"), Seq("Output"), truthTable, a1.value.toSeq ++ b1.value.toSeq).htmlString}
             |
             |It's time to click the logic input buttons to toggle them!
             |""".stripMargin
        ), if (isComplete) <.div(
          Common.marked(
            s"""
               | Notice that when A and B are both `1`, the answer is `1` not `0`. T
               | This is an *inclusive* OR gate (or just "OR" gate). We will later meet an *exclusive* or gate where
               | both inputs being `1` will produce a `0`.
               |
               | But for the moment, let's move on to the logic gate and symbols for (inclusive) OR.
               |""".stripMargin
          ), nextButton()
        )else <.div()
      )
    )(<.div(
      circuit
    )))
  }

}
