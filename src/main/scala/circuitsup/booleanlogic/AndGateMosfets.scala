package circuitsup.booleanlogic

import circuitsup.templates.ExerciseStage
import BooleanTopic.nextButton
import BooleanTopic.onCompletionUpdate
import circuitsup.Common
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.<
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Open}
import com.wbillingsley.wren.Orientation.{East, North, South}
import com.wbillingsley.wren.{Circuit, ConstraintPropagator, Ground, LogicInput, LogicProbe, NMOSSwitch, PMOSSwitch, Terminal, TruthTable, ValueLabel, VoltageSource, Wire}
import org.scalajs.dom.{Element, Node}

import Wire._

object AndGateMosfets extends ExerciseStage {

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

  val nmos1 = new NMOSSwitch(350 -> 100)
  val nmos2 = new NMOSSwitch(350 -> 200)

  val pmos1 = new PMOSSwitch(200 -> 300)
  val pmos2 = new PMOSSwitch(350 -> 300)

  val out = new LogicProbe(400 -> nmos2.source.y, East)

  val wires:Seq[Wire] = Seq(
    (a1.t -> nmos1.gate).wire,
    (b1.t -> nmos2.gate).wire,
    (a2.t -> pmos1.gate).wire,
    (b2.t -> pmos2.gate).wire,
    (gnd.terminal -> pmos1.drain).wireVia(pmos1.drain.x -> gnd.terminal.y),
    (gnd.terminal -> pmos2.drain).wireVia(pmos2.drain.x -> gnd.terminal.y),
    (gnd.terminal -> vdd.t1).wire, (vdd.t2 -> nmos1.drain).wireVia(vdd.t2.x -> nmos1.drain.y),

    (nmos1.source -> nmos2.drain).wire, (nmos2.source -> pmos2.source).wire,
    (pmos1.source -> nmos2.source).wireVia(pmos1.source.x -> nmos2.source.y),
    (nmos2.source -> out.t).wire
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
           |### AND gate in CMOS
           |
           |Let's show a MOSFET circuit for an AND gate, and toggle it to fill in the truth table.
           |
           |There are two inputs marked A and two marked B. If you toggle one A, the other will change with it
           |(we thought that might be less confusing than cluttering the diagram with extra wires).
           |
           |${TruthTable(Seq("A", "B"), truthTable, a1.value.toSeq ++ b1.value.toSeq).htmlString}
           |
           |It's time to click the logic input buttons to toggle them!
           |""".stripMargin
      ), if (isComplete) <.div(
        Common.marked(
          s"""
             | The logic circuit ensures that the output is always connected *either* to V<sub>DD</sub> or to ground
             | (but never both), so we never get a "short circuit" and always get a valid output for valid inputs.
             |
             | And as we can see in the truth table, only the row where A and B are both `1` produces a `1` at the output.
             |""".stripMargin
        ), nextButton()
      )else <.div()
    )
  )(<.div(
    circuit
  )))

}
