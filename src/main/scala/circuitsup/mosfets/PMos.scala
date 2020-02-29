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

object PMos {

  class PMOSFETCircuit {
    val vdd = new VoltageSource((50, 100), North)
    val vgb = new VoltageSource((200, 175), North)
    val gnd = new Ground((50, 350))

    val pMosfet = new PMOSFETB((400, 200), North)

    val j1 = new Terminal((pMosfet.source.x, 50), Some(0))
    val j2 = new Terminal((vgb.t2.x, 50), Some(0))

    val wires:Seq[Wire] = Seq(
      (vdd.t1 -> gnd.terminal).wire,
      (gnd.terminal -> pMosfet.drain).wireVia(pMosfet.drain.x -> gnd.terminal.y),
      (vgb.t2 -> j2).wire, (j1 -> j2).wire,
      (vdd.t2 -> j2).wireVia(vdd.t2.x -> 50),
      (pMosfet.source -> j1).wire,
      (j1 -> pMosfet.body).wireVia(pMosfet.body.x -> 50),
      (vgb.t1 -> pMosfet.gate).wireVia(vgb.t2.x -> pMosfet.gate.y)
    )

    val labels = Seq(
      new ValueLabel("V" -> "DD", vdd.voltage, 80 -> vdd.y),
      new ValueLabel("V" -> "BG", vgb.voltage, 170 -> vgb.y, "right"),
      new ValueLabel("I" -> "SD", pMosfet.drain.current, 330 -> 90, "right", symbol=Seq(ValueLabel.currentArrow(340 -> 80, South))),

      new Label("Drain", (pMosfet.drain.x - 10, pMosfet.drain.y + 10), cssClass = "right"),
      new Label("Gate", (pMosfet.gate.x - 10, pMosfet.gate.y + 20), cssClass = "right"),
      new Label("Source", (pMosfet.source.x - 10, pMosfet.source.y + 20), cssClass = "right"),
      new Label("Body", (pMosfet.body.x + 10, pMosfet.body.y - 20)),
    )

    def components = Seq(pMosfet, vdd, vgb, gnd, j2, j1) ++ wires ++ labels

  }

  object Page1 extends ExerciseStage {
    var completion: Challenge.Completion = Open

    val nmc = new PMOSFETCircuit()
    nmc.vgb.voltage.content = Some(0d -> QuestionSet)

    val circuit = new Circuit(nmc.components :+
      new ValueSlider(nmc.vdd.voltage, nmc.vdd.x + 30 -> (nmc.vdd.y + 10), max="5", min="0", step="0.1")(() => onUpdate())
      , 600, 400)
    val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))

    val plotData = new ScatterPlotData(nmc.vdd.voltage, nmc.pMosfet.drain.current)

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
             |### P-Channel MOSFET
             |
             |Let's switch over from looking at N-Channel MOSFETs to looking at P-Channel MOSFETs
             |
             |On the right, we have a model of an "P-Channel enhancement mode MOSFET" wired up so we can play with
             |what happens to the the current through it.
             |
             |Because this is a "P-Channel" MOSFET, the carriers are **p**ositively charged "holes".
             |The "source" terminal is the source of these "holes", and it is connected to the positive voltage.
             |(However, that is still often called "V<sub>DD</sub>" even though it's connected to a P-Channel MOSFET's source terminal. Oh well.)
             |
             |The semiconductor at the source terminal is "p-doped" (here, shown in green).
             |
             |The "drain" terminal is where the "holes" drain out of the MOSFET. Here, that's connected to the low voltage.
             |It also uses "p-doped" semiconductor (green).
             |
             |The body of the MOSFET, shown in grey, is un-doped semiconductor. We've connected it to the high voltage.
             |
             |V<sub>BG</sub> is the voltage between the body and the gate of the MOSFET. Here, we've set it to zero.
             |You should see that the body of the MOSFET is entirely grey - there is no conducting channel.
             |
             |Try moving the slider for V<sub>DD</sub>, and see what happens to the current through the MOSFET, I<sub>SD</sub>.
             |
             |""".stripMargin
        ),
        if (isComplete) <.div(
          Common.marked(
            """
              | With the gate voltage high (no voltage difference between the body and gate), there's no conduction channel,
              | so regardless of what voltage we apply to V<sub>DD</sub>, it won't conduct. The switch is off.
              |""".stripMargin
          ), nextButton()
        ) else <.div()
      )
    )(<.div(
      circuit,
      ScatterPlot(600, 300, "Vdd", "Current", (d) => nmc.vdd.voltage.stringify(d), (d) => nmc.pMosfet.drain.current.stringify(d), 5,0.005).plot(plotData.dataValues())
    )))
  }

  object Page2 extends ExerciseStage {
    var completion: Challenge.Completion = Open

    val nmc = new PMOSFETCircuit()
    nmc.vgb.voltage.content = Some(4d -> QuestionSet)
    nmc.vdd.voltage.content = Some(5d -> UserSet)

    val circuit = new Circuit(nmc.components :+
      new ValueSlider(nmc.vdd.voltage, nmc.vdd.x + 30 -> (nmc.vdd.y + 10), max="5", min="0", step="0.1")(() => onUpdate()),600, 400)
    val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
    propagator.resolve()

    val plotData = new ScatterPlotData(nmc.vdd.voltage, nmc.pMosfet.drain.current)

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
             |### P-Channel MOSFET
             |
             |This time, we've got the same circuit, but we've taken the gate potential 4V *below* the body's
             |potential. This voltage between the body and the gate is more than the threshold voltage, so the MOSFET
             |should turn on.
             |
             |You should notice that the electric field that's created inside the MOSFET has made a narrow green
             |conductance channel in the body of the MOSFET.
             |
             |Now try moving the slider for V<sub>DD</sub>, and see what happens to the current through the MOSFET, I<sub>SD</sub>.
             |You should see some current flow, just as we saw with the N-Channel MOSFET when it was turned on.
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
              | Again, in digital circuits we don't need to worry much about this. Our voltages are going to be either high or
              | low, and we don't want much current to flow anyway (it generates heat).
              |""".stripMargin
          ), nextButton()
        )else <.div()
      )
    )(<.div(
      circuit,
      ScatterPlot(600, 300, "Vdd", "Current", (d) => nmc.vdd.voltage.stringify(d), (d) => nmc.pMosfet.drain.current.stringify(d), 5,0.005).plot(plotData.dataValues())
    )))
  }

  object Page3 extends ExerciseStage {
    var completion: Challenge.Completion = Open

    val nmc = new PMOSFETCircuit()
    nmc.vdd.voltage.content = Some(3d -> UserSet)
    nmc.vgb.voltage.content = Some(3d -> UserSet)

    val vg = new ValueLabel("V" -> "g", nmc.pMosfet.gate.potential, 200 -> 275, "centre", symbol=Seq(ValueLabel.voltageMarkers(200 -> 220, 200 -> 330)))


    val circuit = new Circuit(nmc.components :+
      new ValueSlider(nmc.vdd.voltage, nmc.vdd.x + 30 -> (nmc.vdd.y + 10), max="5", min="0", step="0.1")(() => onUpdate()) :+
      new ValueSlider(nmc.vgb.voltage, nmc.vgb.x - 130 -> (nmc.vgb.y + 10), max="5", min="0", step="0.1")(() => {
        plotData.data.clear()
        plotData.update()
        onUpdate()
      }) :+ vg, 600, 400)
    val propagator = new ConstraintPropagator(circuit.components.flatMap(_.constraints))
    propagator.resolve()

    val plotData = new ScatterPlotData(nmc.vdd.voltage, nmc.pMosfet.drain.current)

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
             |### P-Channel MOSFET
             |
             |This time, we've given you control of both the gate voltage and V<sub>DD</sub> to play around with.
             |
             |The chart still charts V<sub>DD</sub> against current, but it'll reset as you change the gate voltage.
             |This should let you draw the chart, then alter the gate voltage, and draw it again to see how it's changed.
             |
             |As the potential at the gate is reduced (the voltage between the body and the gate rises), more and more current can flow as the
             |channel gets wider. If the gate is less than 1.2V below the body, though, nothing can flow at all.
             |
             |In a digital circuit, that lets it act as an electronic switch. To turn it on, we set the gate potential *low*.
             |To turn it off, we set the gate potential *high*.
             |""".stripMargin
        ), nextButton()
      )
    )(<.div(
      circuit,
      ScatterPlot(600, 300, "Vdd", "Current", (d) => nmc.vdd.voltage.stringify(d), (d) => nmc.pMosfet.drain.current.stringify(d), 5,0.005).plot(plotData.dataValues())
    )))
  }


  object Page4 extends ExerciseStage {
    var completion: Challenge.Completion = Open

    val vdd = new VoltageSource(50 ->100, North, Some(5d))
    val gnd = new Ground(50, 300)
    val vg = new VoltageSource(200 ->250, North)
    val j = new Terminal(vg.t2.x -> gnd.terminal.y, Some(0))
    val pmos = new PMOSSwitch(350 -> 150)
    val r1 = new Resistor(350 -> 250, North, r=Some(47000d))
    val tout = new Terminal((pmos.drain.x + 30 -> pmos.drain.y), i=Some(0))

    val wires:Seq[Wire] = Seq(
      (vdd.t2 -> pmos.source).wireVia(vdd.t2.x -> 50, pmos.source.x -> 50),
      (vdd.t1 -> gnd.terminal).wire,
      (gnd.terminal -> j).wire, (vg.t1 -> j).wire, (vg.t2 -> pmos.gate).wireVia(vg.t2.x -> pmos.gate.y),
      (r1.t2 -> pmos.drain).wire,
      (pmos.drain -> tout).wire,
      (r1.t1 -> j).wireVia(r1.t1.x -> j.y)
    )

    val labels = Seq(
      new ValueLabel("V" -> "DD", vdd.voltage, (vdd.x + 30) -> vdd.y),
      new ValueLabel("V" -> "g", vg.voltage, (vg.x - 30) -> vg.y, "right"),
      new ValueLabel("R" -> " ", r1.resistance, (r1.x - 30) -> r1.y, "right"),
      new ValueLabel("I" -> " ", pmos.source.current, (pmos.source.x + 30) -> 75, symbol=Seq(ValueLabel.currentArrow((pmos.source.x + 20) -> 75, South))),
      new ValueLabel("V" -> "out", tout.potential, (tout.x + 10) -> r1.y, symbol=Seq(ValueLabel.voltageMarkers((tout.x + 10) -> tout.y, (tout.x + 10 -> gnd.terminal.y)))),
      new ValueSlider(vg.voltage, (vg.x - 130) -> (vg.y + 10), min="0", max="5", step="5")(onUpdate = () => onUpdate()),

      new Label("Source", (pmos.source.x + 10) -> (pmos.source.y + 10)),
      new Label("Gate", (pmos.gate.x - 5) -> (pmos.gate.y - 10), "right"),
      new Label("Drain", (pmos.drain.x - 10) -> (pmos.drain.y), "right")

    )

    val circuit = new Circuit(Seq(vdd, vg, j, pmos, r1, tout, gnd) ++ wires ++ labels, 600, 400)
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
             |### PMOS circuit
             |
             |Enough of the internals of an P-Channel MOSFET. From here on, we can just treat it like an electronic switch that
             |only conducts when the gate potential is *low* (below the body potential).
             |
             |On the right, we've put the P-Channel MOSFET into a circuit and labelled the terminals.
             |You'll notice that for a P-Channel MOSFET, the gate terminal has a circle on it. Remember that circle.
             |That circle means turn the potential *low* to turn this on.
             |
             |Again there's no terminal for the body connection. On an integrated circuit (a silicon chip),
             |there might be billions of PMOSFETs, and typically their bodies are all connected to the positive voltage (V<sub>DD</sub>, which here is 5V).
             |The symbol I've shown for the MOSFET is for a "P-Channel enhancement mode MOSFET with no (separate) body terminal".
             |
             |Below the MOSFET, we have a resistor connecting the drain to 0V. This is called a "pull-down" resistor because
             |when the MOSFET is off, it will pull the output voltage down to 0V.
             |
             |You've been given a slider for the gate potential, but it only has two positions: low and high (off and on).
             |Toggle the gate potential and see what happens to the output voltage and the current through the resistor.
             |
             |* Toggled V<sub>g</sub> low: $vlowDone
             |* Toggled V<sub>g</sub> high: $vhiDone
             |
             |""".stripMargin
        ), if (isComplete) <.div(
          Common.marked(
            """
              | When V<sub>g</sub> is high, the MOSFET won't conduct. The resistor pulls the output voltage low.
              | No current flows because there's no connection between V<sub>DD</sub> and ground - we haven't connected
              | anything to the output terminal to draw current and the MOSFET is off and won't conduct.
              |
              | When V<sub>g</sub> is low, the MOSFET turns on (current can flow) and it pulls the output high.
              | The resistor is fairly large, so not much current flows, but some does.
              |
              | When V<sub>g</sub> is high, V<sub>out</sub> is low. When V<sub>g</sub> is low, V<sub>out</sub> is high.
              | So, this circuit is an "inverter". It's a "PMOS inverter" because it uses a P-Channel MOSFET to make it.
              |""".stripMargin
          ), nextButton()
       )else <.div()
      )
    )(<.div(
      circuit
    )))
  }

}
