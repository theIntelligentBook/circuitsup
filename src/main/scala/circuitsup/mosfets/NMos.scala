package circuitsup.mosfets

import Topic2.{nextButton, onCompletionUpdate}
import circuitsup.Common
import circuitsup.analog.Ohms.ScatterPlotData
import circuitsup.templates.{ExerciseStage, ScatterPlot}
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.<
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Open}
import com.wbillingsley.wren.Orientation.{East, North, South}
import com.wbillingsley.wren.{Circuit, ConstraintPropagator, Ground, Label, NMOSFETB, NMOSSwitch, QuestionSet, Resistor, Terminal, UserSet, ValueLabel, ValueSlider, VoltageSource, Wire}
import org.scalajs.dom.{Element, Node}
import com.wbillingsley.wren.Wire._

object NMos {

  class NMOSFETCircuit {
    val vdd = new VoltageSource((50, 100), North)
    val vgb = new VoltageSource((270, 250), North)
    val gnd = new Ground((50, 350))

    val nMosfet = new NMOSFETB((400, 200), North)

    val j2 = new Terminal((vgb.t1.x, gnd.terminal.y), Some(0))
    val j3 = new Terminal((nMosfet.source.x, gnd.terminal.y), Some(0))

    val wires:Seq[Wire] = Seq(
      (vdd.t1 -> gnd.terminal).wire,
      (gnd.terminal -> j2).wire, (vgb.t1 -> j2).wire,
      (j2 -> j3).wire, (nMosfet.source -> j3).wire,
      (j3 -> nMosfet.body).wireVia(nMosfet.body.x -> gnd.terminal.y),
      (vdd.t2 -> nMosfet.drain).wireVia(vdd.t2.x -> 50, nMosfet.drain.x -> 50),
      (vgb.t2 -> nMosfet.gate).wireVia(vgb.t2.x -> nMosfet.gate.y)
    )

    val labels = Seq(
      new ValueLabel("V" -> "DD", vdd.voltage, 80 -> vdd.y),
      new ValueLabel("V" -> "GB", vgb.voltage, 230 -> vgb.y, "right"),
      new ValueLabel("I" -> "DS", nMosfet.drain.current, 330 -> 90, "right", symbol=Seq(ValueLabel.currentArrow(340 -> 80, South))),

      new Label("Drain", (nMosfet.drain.x - 10, nMosfet.drain.y + 10), cssClass = "right"),
      new Label("Gate", (nMosfet.gate.x - 10, nMosfet.gate.y + 20), cssClass = "right"),
      new Label("Source", (nMosfet.source.x - 10, nMosfet.source.y + 20), cssClass = "right"),
      new Label("Body", (nMosfet.body.x + 10, nMosfet.body.y - 20)),
    )

    def components = Seq(nMosfet, vdd, vgb, gnd, j2, j3) ++ wires ++ labels

  }

  object Page1 extends ExerciseStage {
    var completion: Challenge.Completion = Open

    val nmc = new NMOSFETCircuit()
    nmc.vgb.voltage.content = Some(0d -> QuestionSet)

    val circuit = new Circuit(nmc.components :+
      new ValueSlider(nmc.vdd.voltage, nmc.vdd.x + 30 -> (nmc.vdd.y + 10), max="5", min="0", step="0.1")(() => onUpdate())
      , 600, 400)
    val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))

    val plotData = new ScatterPlotData(nmc.vdd.voltage, nmc.nMosfet.drain.current)

    def checkCompletion:Boolean = plotData.dataValues().length > 25

    def onUpdate():Unit = {
      propagator.clearCalculations()
      propagator.resolve()
      plotData.update()
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
             |### N-Channel MOSFET
             |
             |In a digital circuit, MOSFETs act like digitally controlled switches. They are intriguing devices on their
             |own, however.
             |
             |On the right, we have a model of an "N-Channel enhancement mode MOSFET" wired up so we can play with
             |what happens to the the current through it.
             |
             |Because this is an "N-Channel" MOSFET, the charge carriers flowing through it are **n**egatively charged electrons.
             |The "source" terminal is the source of these electons, and it is connected to the low voltage.
             |Here, that's the reference ground, sometimes called "V<sub>SS</sub>" because it's connected to the **s**ource terminal in nMOS circuits.
             |The semiconductor at the source terminal is "n-doped" (here, shown in pink).
             |
             |The "drain" terminal is where the electrons drain out of the MOSFET. Here, that's connected to the high voltage, V<sub>DD</sub>.
             |(The "DD" in V<sub>DD</sub> comes from it being connected to the **d**rain in NMOS circuits). It also uses "n-doped" semiconductor (pink).
             |
             |The body of the MOSFET, shown in grey, is un-doped semiconductor (grey). We've connected it to the low voltage (ground).
             |
             |V<sub>GB</sub> is the voltage between the gate and the body of the MOSFET. Here, we've set it to zero.
             |You should see that the body of the MOSFET is entirely grey - there is no conducting channel.
             |
             |Try moving the slider for V<sub>DD</sub>, and see what happens to the current through the MOSFET, I<sub>DS</sub>.
             |
             |""".stripMargin
        ),
        if (isComplete) <.div(
          Common.marked(
            """
              | With the gate voltage low, there's no conduction channel, so regardless of what voltage we apply to
              | V<sub>DD</sub>, it won't conduct. The switch is off.
              |""".stripMargin
          ), nextButton()
        ) else <.div()
      )
    )(<.div(
      circuit,
      ScatterPlot(600, 300, "Vdd", "Current", (d) => nmc.vdd.voltage.stringify(d), (d) => nmc.nMosfet.drain.current.stringify(d), 5,0.005).plot(plotData.dataValues())
    )))
  }

  object Page2 extends ExerciseStage {
    var completion: Challenge.Completion = Open

    val nmc = new NMOSFETCircuit()
    nmc.vgb.voltage.content = Some(4d -> QuestionSet)

    val circuit = new Circuit(nmc.components :+
      new ValueSlider(nmc.vdd.voltage, nmc.vdd.x + 30 -> (nmc.vdd.y + 10), max="5", min="0", step="0.1")(() => onUpdate())
      , 600, 400)
    val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
    propagator.resolve()

    val plotData = new ScatterPlotData(nmc.vdd.voltage, nmc.nMosfet.drain.current)

    def checkCompletion:Boolean = plotData.dataValues().length > 25

    def onUpdate():Unit = {
      propagator.clearCalculations()
      propagator.resolve()
      plotData.update()
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
             |### n-Channel MOSFET
             |
             |This time, we've got the same circuit, but I've increased the gate voltage V<sub>GB</sub> to 4V, which is
             |well above the MOSFET's "threshold voltage".
             |
             |You might notice in the diagram that the electric field that's created inside the MOSFET has made a narrow vertical pink
             |conductance channel in the body of the MOSFET, between the source and the drain.
             |
             |Now try moving the slider for V<sub>DD</sub>, and see what happens to the current through the MOSFET, I<sub>DS</sub>.
             |You should see some current flow, but it won't behave exactly like a resistor.
             |
             |
             |""".stripMargin
        ),
        if (isComplete) <.div(
          Common.marked(
            """
              | When V<sub>DD</sub> is low, the MOSFET acts like a resistor. This is called the "linear region". But
              | as the voltage gets higher, the conduction channel "saturates" and the current levels out.
              |
              | In digital circuits we don't need to worry much about this. Our voltages are going to be either high or
              | low, and we don't want much current to flow anyway (it generates heat).
              |""".stripMargin
          ), nextButton()
        )else <.div()
      )
    )(<.div(
      circuit,
      ScatterPlot(600, 300, "Vdd", "Current", (d) => nmc.vdd.voltage.stringify(d), (d) => nmc.nMosfet.drain.current.stringify(d), 5,0.005).plot(plotData.dataValues())
    )))
  }

  object Page3 extends ExerciseStage {
    var completion: Challenge.Completion = Open

    val nmc = new NMOSFETCircuit()
    nmc.vdd.voltage.content = Some(3d -> UserSet)
    nmc.vgb.voltage.content = Some(3d -> UserSet)

    val circuit = new Circuit(nmc.components :+
      new ValueSlider(nmc.vdd.voltage, nmc.vdd.x + 30 -> (nmc.vdd.y + 10), max="5", min="0", step="0.1")(() => onUpdate()) :+
      new ValueSlider(nmc.vgb.voltage, nmc.vgb.x - 130 -> (nmc.vgb.y + 10), max="5", min="0", step="0.1")(() => {
        plotData.data.clear()
        plotData.update()
        onUpdate()
      }), 600, 400)
    val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
    propagator.resolve()

    val plotData = new ScatterPlotData(nmc.vdd.voltage, nmc.nMosfet.drain.current)

    def checkCompletion:Boolean = plotData.dataValues().length > 25

    def onUpdate():Unit = {
      propagator.clearCalculations()
      propagator.resolve()
      plotData.update()
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
             |### n-Channel MOSFET
             |
             |This time, we've given you control of both the gate voltage and V<sub>DD</sub> to play around with.
             |
             |The chart still charts V<sub>DD</sub> against current, but it'll reset as you change the gate voltage.
             |This should let you draw the chart, then alter the gate voltage, and draw it again to see how it's changed.
             |
             |As the gate voltage rises above the threshold voltage (1.2V), more and more current can flow as the
             |channel gets wider (you should see the pink channel get wider). Below 1.2V, though, nothing can flow at all.
             |
             |In a digital circuit, that lets it act as an electronic switch. To turn it on, we set the gate voltage high.
             |To turn it off, we set the gate voltage low.
             |""".stripMargin
        ), nextButton()
      )
    )(<.div(
      circuit,
      ScatterPlot(600, 300, "Vdd", "Current", (d) => nmc.vdd.voltage.stringify(d), (d) => nmc.nMosfet.drain.current.stringify(d), 5,0.005).plot(plotData.dataValues())
    )))
  }


  object Page4 extends ExerciseStage {
    var completion: Challenge.Completion = Open

    val vdd = new VoltageSource(50 ->100, North, Some(5d))
    val gnd = new Ground(50, 300)
    val vg = new VoltageSource(200 ->250, North)
    val j = new Terminal(vg.t2.x -> gnd.terminal.y, Some(0))
    val r1 = new Resistor(300 -> 100, North, r=Some(47000d))
    val nmos = new NMOSSwitch(300 -> 200)
    val tout = new Terminal((nmos.drain.x + 30 -> nmos.drain.y), i=Some(0))

    val wires:Seq[Wire] = Seq(
      (vdd.t2 -> r1.t2).wireVia(vdd.t2.x -> 50, r1.t1.x -> 50),
      (vdd.t1 -> gnd.terminal).wire,
      (gnd.terminal -> j).wire, (vg.t1 -> j).wire, (vg.t2 -> nmos.gate).wireVia(vg.t2.x -> nmos.gate.y),
      (r1.t1 -> nmos.drain).wire,
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

      new Label("Source", (nmos.source.x + 10) -> (nmos.source.y + 10)),
      new Label("Gate", (nmos.gate.x - 5) -> (nmos.gate.y - 10), "right"),
      new Label("Drain", (nmos.drain.x - 10) -> (nmos.drain.y), "right")

    )

    val circuit = new Circuit(Seq(vdd, vg, j, nmos, r1, tout, gnd) ++ wires ++ labels, 600, 400)
    val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
    propagator.resolve()

    var vlowDone = false
    var vhiDone = false

    def checkCompletion:Boolean = vlowDone && vhiDone

    def onUpdate():Unit = {
      propagator.clearCalculations()
      propagator.resolve()

      if (vg.voltage <= 1) vlowDone = true
      if (vg.voltage >= 3) vhiDone = true

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
             |### nMOS circuit
             |
             |Enough of the internals of an n-MOSFET. From here on, we can just treat it like an electronic switch that
             |only conducts when a voltage is applied to the gate.
             |
             |On the right, we've put the nMOSFET into a circuit and labelled the terminals.
             |
             |You'll notice there's no terminal for the body connection. On an integrated circuit (a silicon chip),
             |there might be billions of nMOSFETs, and typically their bodies are all connected to 0V. The symbol
             |I've shown for the MOSFET is the one for an "n-Channel enhancement mode MOSFET with no (separate) body terminal".
             |
             |Above the MOSFET, we have a resistor connecting the drain to 5V. This is called a "pull-up" resistor because
             |when the MOSFET is off, it will pull the output voltage up to 5V.
             |
             |You've been given a slider for the gate voltage, but it only has two positions: low and high (off and on).
             |Toggle the gate voltage and see what happens to the output voltage and the current through the resistor.
             |
             |* Toggled V<sub>g</sub> low: $vlowDone
             |* Toggled V<sub>g</sub> high: $vhiDone
             |
             |""".stripMargin
        ), if (isComplete) <.div(
          Common.marked(
            """
              | When V<sub>g</sub> is low, the MOSFET won't conduct. The resistor pulls the output voltage high.
              | No current flows because there's no connection between V<sub>DD</sub> and ground - we haven't connected
              | anything to the output terminal to draw current and the MOSFET is off and won't conduct.
              |
              | When V<sub>g</sub> is high, the MOSFET turns on (current can flow) and it pulls the output low.
              | The resistor is fairly large, so not much current flows, but some does.
              |
              | When V<sub>g</sub> is high, V<sub>out</sub> is low. When V<sub>g</sub> is low, V<sub>out</sub> is high.
              | So, this circuit is an "inverter". It's an "nMOS inverter" because it uses an n-channel MOSFET to make it.
              |""".stripMargin
          ), nextButton()
       )else <.div()
      )
    )(<.div(
      circuit
    )))
  }

}
