package circuitsup.mosfets

import circuitsup.Common
import circuitsup.analog.Ohms.ScatterPlotData
import circuitsup.mosfets.Topic2.{nextButton, onCompletionUpdate}
import circuitsup.templates.{ExerciseStage, ScatterPlot}
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.<
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Open}
import com.wbillingsley.wren.Orientation.{East, North, South}
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

    def checkCompletion:Boolean = doneLow && doneHigh

    var doneLow = false
    var doneHigh = false

    def onUpdate():Unit = {
      propagator.clearCalculations()
      propagator.resolve()

      doneLow = doneLow || vg.voltage <= LogicProbe.max0
      doneHigh = doneHigh || vg.voltage >= LogicProbe.min1

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
             |* If the voltage is low, ${ if (doneLow) "the PMOSFET is on and the NMOSFET is off. V<sub>out</sub> is connected to V<sub>DD</sub>, so it is 5V" else "..?" }
             |* If the voltage is high, ${if (doneHigh) "the PMOSFET is off and the NMOSFET is on. V<sub>out</sub> is connected to ground, so it is 0V" else "..?" }
             |
             |""".stripMargin
        ), if (isComplete) <.div(
          Common.marked(
            s"""
               | In the next stage, we're going to start transitioning away from thinking about the voltage, and start
               | thinking about a low voltage as being logical `0` and a high voltage being logical `1`
               |""".stripMargin
          ), nextButton()
        )else <.div()
      )
    )(<.div(
      circuit
    )))
  }

  object Page3 extends ExerciseStage {

    implicit val wireCol = Wire.voltageColoring
    implicit val nMosCol = NMOSSwitch.voltageColouring
    implicit val pMosCol = PMOSSwitch.voltageColouring

    var completion: Challenge.Completion = Open

    val vdd = new VoltageSource(50 ->100, North, Some(5d))
    val gnd = new Ground(50, 300)
    val in = new LogicInput(200 ->200, East)(_ => onUpdate())

    val pmos = new PMOSSwitch(350 -> 125)
    val nmos = new NMOSSwitch(350 -> 225)

    val j = new Terminal(250 -> 200, Some(0))
    val out = new LogicProbe(pmos.drain.x + 30 -> pmos.drain.y, East)

    val wires:Seq[Wire] = Seq(
      (vdd.t2 -> pmos.source).wireVia(vdd.t2.x -> 50, pmos.source.x -> 50),
      (vdd.t1 -> gnd.terminal).wire,
      (gnd.terminal -> nmos.source).wireVia(nmos.source.x -> gnd.terminal.y),
      (in.t -> j).wire, (j -> pmos.gate).wireVia(j.x-> pmos.gate.y), (j -> nmos.gate).wireVia(j.x -> nmos.gate.y),
      (pmos.drain -> out.t).wire, (pmos.drain -> nmos.drain).wire
    )

    val labels = Seq(
      new ValueLabel("V" -> "DD", vdd.voltage, (vdd.x + 30) -> vdd.y),
      new ValueLabel("I" -> " ", pmos.source.current, (pmos.source.x + 30) -> 75, symbol=Seq(ValueLabel.currentArrow((pmos.source.x + 20) -> 75, South))),
      new ValueLabel("V" -> "out", out.t.potential, (out.t.x + 10) -> nmos.y),
    )

    val circuit = new Circuit(Seq(vdd, in, j, pmos, nmos, out, gnd) ++ wires ++ labels, 600, 400)
    val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
    propagator.resolve()

    def checkCompletion:Boolean = truthTable.size >= 2

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
        a <- in.value
        b <- out.value
      } truthTable = truthTable.updated(Seq(a), b)

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
             |### Our first CMOS Gate
             |
             |Let's replace the voltage source at the gate with a logic input.
             |I've shown it as a box with a double rounded border. At the moment, it's not set.
             |
             |Let's also attach a logic probe at the output (the square box with a '?')
             |
             |Your task in this exercise is to toggle the logic input to 0 and to 1 (by clicking on it). When it is
             |0, its terminal will be set to 0V, and when it is 1 its terminal will be set to V<sub>DD</sub>.
             |The voltages and currents will propagate through the circuit and we'll see what happens to the logic
             |probe on the output.
             |
             |The table below will update with the *truth table* of what output corresponded to what input.
             |
             |${TruthTable(Seq("Input"), truthTable, in.value.toSeq).htmlString}
             |
             |It's time to click the logic input button to toggle it!
             |""".stripMargin
        ), if (isComplete) <.div(
          Common.marked(
            s"""
               | Now that we've filled in the truth table, we can see this makes a *NOT* gate. An input of `1` becomes
               | `0` and an input of `0` becomes `1`.
               |""".stripMargin
          ), nextButton()
        )else <.div()
      )
    )(<.div(
      circuit
    )))
  }


  object Page4 extends ExerciseStage {

    implicit val wireCol = Wire.voltageColoring

    var completion: Challenge.Completion = Open

    val in = new LogicInput(200 ->200, East, "A")(_ => onUpdate())
    val out = new LogicProbe(300 -> 200, East)
    val not = new NotGate(250 -> 200, East)

    val wires:Seq[Wire] = Seq(
      (in.t -> not.in).wire,
      (not.out -> out.t).wire
    )

    val circuit = new Circuit(Seq(in, not, out) ++ wires, 600, 400)
    val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
    propagator.resolve()

    def checkCompletion:Boolean = truthTable.size >= 2

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
        a <- in.value
        b <- out.value
      } truthTable = truthTable.updated(Seq(a), b)

      if (checkCompletion) {
        completion = Complete(Some(1), None)
        onCompletionUpdate()
      } else {
        rerender()
      }
    }

    val watts = new Value("W")

    override protected def render: DiffNode[Element, Node] = {
      import Common.notS

      <.div(Challenge.textAndEx(<.div(
        Common.marked(
          s"""
             |### Our CMOS Not Gate
             |
             |Now that we know the circuit is a NOT gate, let's remove the detail of the circuitry and replace it
             |just with the gate symbol for a NOT gate.
             |
             |Toggle the input again, and you'll see we're still colouring the wires with the voltage level, but in
             |practice we're more interested in the logical operation of the gate. We've abstracted away from
             |MOSFETs and voltages.
             |
             |We can also write the NOT operation symbolically.
             |There are different notations you're likely to come across, most of which have their origins in the 1800s.
             |
             |* <code>&not;A</code> - this is the symbol you're likely to see on mathematical logic courses.
             |  Quine's famous textbook "Methods of Logic" refers to this as "a deviant sign", but mathematicians like it anyway!
             |* <code>\\~A</code> - This notation became popular in Whitehead and Russell's [Principia Mathematica](https://en.wikipedia.org/wiki/Principia_Mathematica).
             |  You might like it for the fact that there is a `~` key on your keyboard. Consequently, many programming languages use `~` as the *bitwise NOT* operator.
             |* <code style="text-decoration: overline">A</code> - engineers use an overbar.
             |  This notiation comes from C.S. Pierce in 1870, but it's useful for its compactness. For example, if you have
             |  three inputs `A`, `B`, and `C`, and you want to discuss the case of `1`, `0`, and `1`, you can compactly write
             |  <code>A${notS("B")}C</code>
             |* <code>A'</code> - an overbar is difficult to type, so engineers will often use this to type it instead. e.g.
             |  `AB'C`. As the "combining overbar" doesn't render correctly in some browsers (and can be hard to distinguish in web
             |  text anyway), we'll use the `A'` notation here too when we want to talk about the complement of a single input.
             |
             |${TruthTable(Seq("Input"), truthTable, in.value.toSeq).htmlString}
             |
             |It's time to click the logic input button to toggle it!
             |""".stripMargin
        ), if (isComplete) <.div(
          Common.marked(
            s"""
               | You might notice there's a circle on the output of the NOT gate, and there was a circle on the input
               | gate of a p-type MOSFET (which turned on if the input was low rather than high). Where you see that circle, think
               | "inversion".
               |""".stripMargin
          ), nextButton()
        ) else <.div()
      ))(<.div(
        circuit
      )))
    }
  }


}
