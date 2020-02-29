package circuitsup.mosfets

import circuitsup.Common
import circuitsup.analog.Ohms.ScatterPlotData
import circuitsup.mosfets.Topic2.{nextButton, onCompletionUpdate}
import circuitsup.templates.{ExerciseStage, ScatterPlot}
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.<
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Open}
import com.wbillingsley.wren.Orientation.{North, South}
import com.wbillingsley.wren.Wire._
import com.wbillingsley.wren._
import org.scalajs.dom.{Element, Node}

object CMos {

  object Page1 extends ExerciseStage {

    var completion: Challenge.Completion = Open

    object pmos {
      val vdd = new VoltageSource(50 ->100, North, Some(5d))
      val gnd = new Ground(50, 300)
      val vg = new VoltageSource(200 ->250, North)
      val j = new Terminal(vg.t2.x -> gnd.terminal.y, Some(0))
      val pmos = new PMOSSwitch(350 -> 150)
      val r1 = new Resistor(350 -> 250, South, r=Some(47000d))
      val tout = new Terminal((pmos.drain.x + 30 -> pmos.drain.y), i=Some(0))

      val wires:Seq[Wire] = Seq(
        (vdd.t2 -> pmos.source).wireVia(vdd.t2.x -> 50, pmos.source.x -> 50),
        (vdd.t1 -> gnd.terminal).wire,
        (gnd.terminal -> j).wire, (vg.t1 -> j).wire, (vg.t2 -> pmos.gate).wireVia(vg.t2.x -> pmos.gate.y),
        (r1.t1 -> pmos.drain).wire,
        (pmos.drain -> tout).wire,
        (r1.t2 -> j).wireVia(r1.t2.x -> j.y)
      )

      val labels = Seq(
        new ValueLabel("V" -> "DD", vdd.voltage, (vdd.x + 30) -> vdd.y),
        new ValueLabel("V" -> "g", vg.voltage, (vg.x - 30) -> vg.y, "right"),
        new ValueLabel("R" -> " ", r1.resistance, (r1.x - 30) -> r1.y, "right"),
        new ValueLabel("I" -> " ", pmos.source.current, (pmos.source.x + 30) -> 75, symbol=Seq(ValueLabel.currentArrow((pmos.source.x + 20) -> 75, South))),
        new ValueLabel("V" -> "out", tout.potential, (tout.x + 10) -> r1.y, symbol=Seq(ValueLabel.voltageMarkers((tout.x + 10) -> tout.y, (tout.x + 10 -> gnd.terminal.y)))),
        new ValueSlider(vg.voltage, (vg.x - 130) -> (vg.y + 10), min="0", max="5", step="5")(onUpdate = () => onUpdate()),

        new Label("Source", (pmos.source.x - 10) -> (pmos.source.y ), "right"),
        new Label("Gate", (pmos.gate.x - 5) -> (pmos.gate.y - 10), "right"),
        new Label("Drain", (pmos.drain.x - 10) -> (pmos.drain.y + 10), "right")

      )

      val circuit = new Circuit(Seq(vdd, vg, j, pmos, r1, tout, gnd) ++ wires ++ labels, 600, 400)
      val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
      propagator.resolve()
    }

    object nmos {
      val vdd = new VoltageSource(50 ->100, North, Some(5d))
      val gnd = new Ground(50, 300)
      val vg = new VoltageSource(200 ->250, North)
      val j = new Terminal(vg.t2.x -> gnd.terminal.y, Some(0))
      val r1 = new Resistor(350 -> 100, South, r=Some(47000d))
      val nmos = new NMOSSwitch(350 -> 200)
      val tout = new Terminal((nmos.drain.x + 30 -> nmos.drain.y), i=Some(0))

      val wires:Seq[Wire] = Seq(
        (vdd.t2 -> r1.t1).wireVia(vdd.t2.x -> 50, r1.t1.x -> 50),
        (vdd.t1 -> gnd.terminal).wire,
        (gnd.terminal -> j).wire, (vg.t1 -> j).wire, (vg.t2 -> nmos.gate).wireVia(vg.t2.x -> nmos.gate.y),
        (r1.t2 -> nmos.drain).wire,
        (nmos.drain -> tout).wire,
        (nmos.source -> j).wireVia(nmos.source.x -> gnd.terminal.y)
      )

      val labels = Seq(
        new ValueLabel("V" -> "DD", vdd.voltage, (vdd.x + 30) -> vdd.y),
        new ValueLabel("V" -> "g", vg.voltage, (vg.x - 30) -> vg.y, "right"),
        new ValueLabel("R" -> " ", r1.resistance, (r1.x - 30) -> r1.y, "right"),
        new ValueLabel("I" -> " ", r1.t2.current, (r1.x + 30) -> r1.y, symbol=Seq(ValueLabel.currentArrow((r1.x + 20) -> r1.y, South))),
        new ValueLabel("V" -> "out", tout.potential, (tout.x + 10) -> nmos.gate.y, symbol=Seq(ValueLabel.voltageMarkers((tout.x + 10) -> tout.y, (tout.x + 10 -> gnd.terminal.y)))),
        new ValueSlider(vg.voltage, (vg.x - 130) -> (vg.y + 10), min="0", max="5", step="5")(onUpdate = () => onUpdate()),

        new Label("Source", (nmos.source.x - 10) -> (nmos.source.y + 10), "right"),
        new Label("Gate", (nmos.gate.x - 5) -> (nmos.gate.y - 10), "right"),
        new Label("Drain", (nmos.drain.x - 10) -> (nmos.drain.y), "right")

      )

      val circuit = new Circuit(Seq(vdd, vg, j, nmos, r1, tout, gnd) ++ wires ++ labels, 600, 400)
      val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
      propagator.resolve()
    }

    def checkCompletion:Boolean = nmos.nmos.drain.current > 0d && pmos.pmos.source.current > 0d

    def onUpdate():Unit = {
      nmos.propagator.clearCalculations()
      nmos.propagator.resolve()

      pmos.propagator.clearCalculations()
      pmos.propagator.resolve()



      if (checkCompletion) {
        completion = Complete(Some(1), None)
        onCompletionUpdate()
      } else {
        rerender()
      }
    }

    val watts = new Value("W")

    override protected def render: DiffNode[Element, Node] = <.div(Challenge.textAndEx(
      <.div(
        Common.marked(
          s"""
             |### The problem with NMOS and PMOS
             |
             |On the right hand side, we've put an NMOS inverter and a PMOS inverter.
             |
             |In the NMOS inverter, if the input is low (the MOSFET is off), there's no current. But if the input is
             |high, a current will flow. That current will generate heat, and we don't want that to happen or our chip will get hot.
             |
             |In the PMOS inverter, if the input is high (the MOSFET is off), there's no current. But if the input is low,
             |a current will flow. That current will generate heat, and we don't want that to happen.
             |
             |Your task in this exercise is to set both inverters so that current flows.
             |
             |The power consumed (turned into heat) is equal to current times voltage. So, in each resistor we have
             |
             |* In the PMOS inverter: ${pmos.r1.t1.current.stringify("?")} &times; ${pmos.r1.voltage.stringify("?")} =
             |  ${(pmos.r1.t1.current * pmos.r1.voltage).map(watts.stringify).getOrElse("?")}
             |
             |* In the NMOS inverter: ${ nmos.r1.t1.current.stringify("?")} &times; ${ nmos.r1.voltage.stringify("?")} =
             |  ${ (nmos.r1.t1.current * nmos.r1.voltage).map(watts.stringify).getOrElse("?") }
             |
             |
             |""".stripMargin
        ), if (isComplete) <.div(
          Common.marked(
            s"""
              | That might not seem a lot, but multiply it by billions of MOSFET circuits in a processor, and it starts
              | to add up to a lot of heat.
              |
              | In the next stage, we're going to solve this problem by using *both* N-Channel *and* P-Channel MOSFETs.
              | Instead of using a pull-up resistor, we'll use a P-Channel MOSFET. Instead of using a pull-down resistor,
              | we'll use an N-Channel MOSFET. One of them will always be turned off, and that means we won't see current
              | flowing through resistors generating so much heat.
              |""".stripMargin
          ), nextButton()
       )else <.div()
      )
    )(<.div(
      pmos.circuit,
      nmos.circuit
    )))
  }

  object Page2 extends ExerciseStage {

    implicit val wireCol = Wire.voltageColoring
    implicit val nMosCol = NMOSSwitch.voltageColouring
    implicit val pMosCol = PMOSSwitch.voltageColouring

    var completion: Challenge.Completion = Open

    val vdd = new VoltageSource(50 ->100, North, Some(5d))
    val gnd = new Ground(50, 300)
    val vg = new VoltageSource(200 ->250, North)
    val pmos = new PMOSSwitch(350 -> 125)
    val nmos = new NMOSSwitch(350 -> 225)

    val j = new Terminal(250 -> 200, Some(0))
    val j2 = new Terminal(vg.t1.x -> gnd.terminal.y, Some(0))
    val tout = new Terminal((pmos.drain.x + 30 -> pmos.drain.y), i=Some(0))


    val wires:Seq[Wire] = Seq(
      (vdd.t2 -> pmos.source).wireVia(vdd.t2.x -> 50, pmos.source.x -> 50),
      (vdd.t1 -> gnd.terminal).wire,
      (gnd.terminal -> j2).wire, (j2 -> vg.t1).wire, (j2 -> nmos.source).wireVia(nmos.source.x -> j2.y),
      (vg.t2 -> j).wireVia(vg.t2.x -> j.y), (j -> pmos.gate).wireVia(j.x-> pmos.gate.y), (j -> nmos.gate).wireVia(j.x -> nmos.gate.y),
      (pmos.drain -> tout).wire, (pmos.drain -> nmos.drain).wire
    )

    val labels = Seq(
      new ValueLabel("V" -> "DD", vdd.voltage, (vdd.x + 30) -> vdd.y),
      new ValueLabel("V" -> "g", vg.voltage, (vg.x - 30) -> vg.y, "right"),
      new ValueLabel("I" -> " ", pmos.source.current, (pmos.source.x + 30) -> 75, symbol=Seq(ValueLabel.currentArrow((pmos.source.x + 20) -> 75, South))),
      new ValueLabel("V" -> "out", tout.potential, (tout.x + 10) -> nmos.y, symbol=Seq(ValueLabel.voltageMarkers((tout.x + 10) -> tout.y, (tout.x + 10 -> gnd.terminal.y)))),
      new ValueSlider(vg.voltage, (vg.x - 130) -> (vg.y + 10), min="0", max="5", step="5")(onUpdate = () => onUpdate())


    )

    val circuit = new Circuit(Seq(vdd, vg, j, j2, pmos, nmos, tout, gnd) ++ wires ++ labels, 600, 400)
    val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
    propagator.resolve()

    def checkCompletion:Boolean = false

    def onUpdate():Unit = {
      propagator.clearCalculations()
      propagator.resolve()



      if (checkCompletion) {
        completion = Complete(Some(1), None)
        onCompletionUpdate()
      } else {
        rerender()
      }
    }

    val watts = new Value("W")

    override protected def render: DiffNode[Element, Node] = <.div(Challenge.textAndEx(
      <.div(
        Common.marked(
          s"""
             |### Complementary MOS (CMOS)
             |
             |On the right hand side, we've now got an inverter that uses *both* an N-Channel MOSFET and a P-Channel MOSFET.
             |
             |We've also set it so that it'll colour the wires: blue if they are below 1.2V and red if they are above 1.2V.
             |The MOSFTETs will only colour their channel if they are conducting, so it should be visible which MOSFET is
             |on and which is off.
             |
             |Remember that the circle on the P-Channel MOSFET's gate means "set the voltage low to make this conduct".
             |
             |* If the voltage is high, the PMOSFET is off and the NMOSFET is on. V<sub>out</sub> is connected to ground, so it is 0V
             |
             |* If the voltage is high, the PMOSFET is on and the NMOSFET is off. V<sub>out</sub> is connected to V<sub>DD</sub>, so it is 5V
             |
             |Try both, and see that in either case, the current through the circuit is zero.
             |
             |""".stripMargin
        ), if (isComplete) <.div(
          Common.marked(
            s"""
               | That might not seem a lot, but multiply it by billions of MOSFET circuits in a processor, and it starts
               | to add up to a lot of heat.
               |
               | In the next stage, we're going to solve this problem by using *both* N-Channel *and* P-Channel MOSFETs.
               | Instead of using a pull-up resistor, we'll use a P-Channel MOSFET. Instead of using a pull-down resistor,
               | we'll use an N-Channel MOSFET. One of them will always be turned off, and that means we won't see current
               | flowing through resistors generating so much heat.
               |""".stripMargin
          ), nextButton()
        )else <.div()
      )
    )(<.div(
      circuit
    )))
  }

}
