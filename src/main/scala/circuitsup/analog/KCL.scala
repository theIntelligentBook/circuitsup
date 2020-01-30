package circuitsup.analog

import circuitsup.analog.KCL.page1.checkCompletion
import circuitsup.analog.Ohms.page1.{onCompletionUpdate, rerender}
import circuitsup.{CircuitsRoute, Common, Router}
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.html.{<, VHtmlNode, ^}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Completion, Open}
import com.wbillingsley.wren.Component.ColouringRule
import com.wbillingsley.wren.Orientation.{East, South}
import com.wbillingsley.wren.{Circuit, ConstraintPropagator, CurrentSource, Junction, Orientation, Resistor, Terminal, UserSet, ValueLabel, ValueSlider, VoltageSource, Wire}

object KCL {

  val vd = new VoltageDivider()

  import Analog.{onCompletionUpdate, nextButton}

  object page1 extends ExerciseStage {

    val junction = new OneWireFragment((100, 100), 200)
    val i1 = new ValueLabel("I" -> "1", junction.wire.t1current, (120, 100), symbol = Seq(ValueLabel.currentArrow((110, 100), Orientation.South)))
    val i2 = new ValueLabel("I" -> "2", junction.t2.current, (120, 300), symbol = Seq(ValueLabel.currentArrow((110, 300), Orientation.South)), colouringRule = () => if (checkCompletion) "green" else "")
    val i1slider = new ValueSlider(junction.wire.t1current, (120, 120), min = "0", max = "0.1", step = "0.01")(onUpdate = onUpdate, enabled = !this.isComplete )

    val circuit = new Circuit(i1 +: i1slider +: i2 +: junction.components, 300, 400)

    val propagator = ConstraintPropagator(circuit.components.flatMap(_.constraints))

    def checkCompletion = {
      junction.t2.current.value match {
        case Some((i, _)) if Math.abs(i - 0.02) < 0.002 => true
        case _ => false
      }
    }

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

    var completion:Completion = Open

    def render = <.div(Challenge.textAndEx(
      <.div(
        <.h3("What goes in must come out"),
        <.p(
          """
            |Currents are flows of charge. In "the steady state", we don't have charge building up anywhere in a circuit.
            |So, any current flowing into a node must also flow out.
            |""".stripMargin
        ),
        <.p(
          """
            |On the right hand side, we have a small fragment of a circuit. Assume the wire is connected to other components
            |that are not shown at the top and bottom. We're just considering the current in this one wire.
            |""".stripMargin
        ),
        <.p(
          "You've been given a slider to set I",
          <("sub")("1"), " and your task is to make I", <("sub")("2"), " 20mA"
        ),
        completion match {
          case Complete(_, _) =>
            <.div(
              <.p(
                "That was easy enough! Kirchhoff's Current Law implies that if there's 20mA coming into the wire, there must be 20mA going out of it too.",
                "Let's move on to the next exercise"
              ),
              nextButton()
            )
          case _ => <.div()
        }
      )
    )(
      <.div(
        circuit
      )
    ))

  }

  object page2 extends ExerciseStage {

    val junction = new OneWireFragment((100, 100), 200)
    val i1 = new ValueLabel("I" -> "1", junction.wire.t1current, (120, 100), symbol = Seq(ValueLabel.currentArrow((110, 100), Orientation.South)))
    val i2 = new ValueLabel("I" -> "2", junction.wire.t2current, (120, 300), symbol = Seq(ValueLabel.currentArrow((110, 300), Orientation.North)), colouringRule = () => if (checkCompletion) "green" else "")
    val i1slider = new ValueSlider(junction.wire.t1current, (120, 120), min = "0", max = "0.1", step = "0.01")(onUpdate = onUpdate, enabled = !this.isComplete )

    val circuit = new Circuit(i1 +: i1slider +: i2 +: junction.components, 300, 400)

    val propagator = ConstraintPropagator(circuit.components.flatMap(_.constraints))

    def checkCompletion = {
      junction.wire.t2current.value match {
        case Some((i, _)) if Math.abs(i + 0.05) < 0.002 => true
        case _ => false
      }
    }

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

    var completion:Completion = Open

    def render = <.div(Challenge.textAndEx(
      <.div(
        <.h3("Negative current is current in the other direction"),
        <.p(
          """
            |In the previous exercise, we showed the current coming out of the wire, because that makes more intuitive sense.
            |But Kirchhoff's Current Law is described as the sum of currents into a node.
            |""".stripMargin
        ),
        <.p(
          "This time, on the right hand side, we've labelled a current I", <("sub")("2"), " that is drawn going into the wire. ",
          "Notice the arrow points upwards this time."
        ),
        <.p(
          "You've been given a slider to set I",
          <("sub")("1"), " and your task is to make I", <("sub")("2"), " -50mA"
        ),
        completion match {
          case Complete(_, _) =>
            <.div(
              <.p(
                "So, if 50mA is going into the wire, -50mA into the wire at the other end would sum to zero.",
                "-50mA heading into the wire is equivalent to 50mA heading out of the wire."
              ),
              nextButton()
            )
          case _ => <.div()
        }
      )
    )(
      <.div(
        circuit
      )
    ))

  }


  object page3 extends ExerciseStage {

    val r1 = new Resistor((100, 160), South)
    val r2 = new Resistor((100, 240), South)
    val t1 = new Terminal((100, 100))
    val t2 = new Terminal((100, 300))
    val w1 = new Wire(t1, r1.t1)
    val w2 = new Wire(r1.t2, r2.t1)
    val w3 = new Wire(r2.t2, t2)
    val i1 = new ValueLabel("I" -> "1", w1.t1current, (120, 100), symbol = Seq(ValueLabel.currentArrow((110, 100), Orientation.South)))
    val i2 = new ValueLabel("I" -> "2", r1.t1.current, (120, 300), symbol = Seq(ValueLabel.currentArrow((110, 300), Orientation.South)), colouringRule = () => if (checkCompletion) "green" else "")
    val i1slider = new ValueSlider(t2.current, (120, 120), min = "0", max = "0.1", step = "0.01")(onUpdate = onUpdate, enabled = !this.isComplete )

    val circuit = new Circuit(Seq(t1, t2, w1, w2, w3, i1, i2, r1, r2, i1slider), 300, 400)

    val propagator = ConstraintPropagator(circuit.components.flatMap(_.constraints))

    def checkCompletion = {
      t2.current.value match {
        case Some((i, _)) if Math.abs(i - 0.05) < 0.002 => true
        case _ => false
      }
    }

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

    var completion:Completion = Open

    def render = <.div(Challenge.textAndEx(
      <.div(
        <.h3("It doesn't matter what the component is"),
        <.p(
          """
            |So far we've shown Kirchoff's Current Law for wires, but it holds true for every component and every node.
            |""".stripMargin
        ),
        <.p(
          "This time, on the right hand side, we now have two resistors connected 'in series'. Don't worry about the voltage yet",
          "Let's just apply our basic KCL law to this construction."
        ),
        <.p(
          "You've been given a slider to set I",
          <("sub")("1"), " and your task is to make I", <("sub")("2"), " 50mA"
        ),
        completion match {
          case Complete(_, _) =>
            <.div(
              <.p(
                "Again, if 50mA is coming in, 50mA must be going out."
              ),
              nextButton()
            )
          case _ => <.div()
        }
      )
    )(
      <.div(
        circuit
      )
    ))

  }


  object page4 extends ExerciseStage {

    val r1 = new Resistor((100, 160), South)
    val r2 = new Resistor((100, 240), South)
    val r3 = new Resistor((140, 200), East)
    val j = new Junction((100, 200))
    val t1 = new Terminal((100, 100))
    val t2 = new Terminal((100, 300))
    val t3 = new Terminal((200, 200))

    t1.current.value = Some((-0.1, UserSet))

    val w1 = new Wire(t1, r1.t1)
    val w1j = new Wire(r1.t2, j.terminal)
    val w2j = new Wire(j.terminal, r2.t1)
    val w3j = new Wire(j.terminal, r3.t1)
    val w2 = new Wire(r2.t2, t2)
    val w3 = new Wire(r3.t2, t3)

    val i1 = new ValueLabel("I" -> "1", w1.t1current, (120, 100), symbol = Seq(ValueLabel.currentArrow((110, 100), Orientation.South)))
    val i2 = new ValueLabel("I" -> "2", t2.current, (120, 300), symbol = Seq(ValueLabel.currentArrow((110, 300), Orientation.South)))
    val i3 = new ValueLabel("I" -> "3", t3.current, (180, 175), symbol = Seq(ValueLabel.currentArrow((190, 190), Orientation.East)), colouringRule = () => if (checkCompletion) "green" else "")
    val i2slider = new ValueSlider(t2.current, (120, 320), min = "0", max = "0.1", step = "0.01")(onUpdate = onUpdate, enabled = !this.isComplete )

    val circuit = new Circuit(Seq(t1, t2, t3, j, w1, w2, w3, w1j, w2j, w3j, i1, i2, i3, r1, r2, r3, i2slider), 300, 400)

    val propagator = ConstraintPropagator(circuit.components.flatMap(_.constraints))
    propagator.resolve()

    def checkCompletion = {
      t3.current.value match {
        case Some((i, _)) if Math.abs(i - 0.05) < 0.002 => true
        case _ => false
      }
    }

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

    var completion:Completion = Open

    def render = <.div(Challenge.textAndEx(
      <.div(
        <.h3("KCL with three paths"),
        <.p(
          """
            |In our examples so far, there have only been two currents into (or out of) the circuit fragment to deal with.
            |Let's try one with three.
            |""".stripMargin
        ),
        <.p(
          "This time, on the right hand side, we now have three resistors connected to a junction."
        ),
        <.p(
          "You've been given a slider to set I",
          <("sub")("2"), " and your task is to make I", <("sub")("3"), " 50mA"
        ),
        completion match {
          case Complete(_, _) =>
            <.div(
              <.p(
                "Well done. Adjusting I", <("sub")("2"), " has left 50mA over for I", <("sub")("3")
              ),
              nextButton()
            )
          case _ => <.div()
        }
      )
    )(
      <.div(
        circuit
      )
    ))

  }


  object page5 extends ExerciseStage {

    val xa = 100
    val xb = 250
    val xc = 400
    val xd = 500

    val ta1 = new Terminal((xa, 100))
    val ta2 = new Terminal((xa, 300))
    val tb1 = new Terminal((xb, 100))
    val tb2 = new Terminal((xb, 300))
    val tc1 = new Terminal((xc, 100))
    val tc2 = new Terminal((xc, 300))
    val tout = new Terminal((xd, 200))

    val ra1 = new Resistor((xa, 160), South)
    val ra2 = new Resistor((xa, 240), South)
    val rb1 = new Resistor((xb, 160), South)
    val rb2 = new Resistor((xb, 240), South)
    val rc1 = new Resistor((xc, 160), South)
    val rc2 = new Resistor((xc, 240), South)


    val rout = new Resistor((440, 200), East)
    val ja = new Junction((xa, 200))
    val jb = new Junction((xb, 200))
    val jc = new Junction((xc, 200))



    val wjab = new Wire(ja.terminal, jb.terminal)
    val wjbc = new Wire(jb.terminal, jc.terminal)
    val wires:Seq[Wire] = Seq(wjab, wjbc) ++ Seq(
      ta1 -> ra1.t1, ra1.t2 -> ja.terminal, ja.terminal -> ra2.t1, ra2.t2 -> ta2,
      tb1 -> rb1.t1, rb1.t2 -> jb.terminal, jb.terminal -> rb2.t1, rb2.t2 -> tb2,
      tc1 -> rc1.t1, rc1.t2 -> jc.terminal, jc.terminal -> rc2.t1, rc2.t2 -> tc2,
      jc.terminal -> rout.t1, rout.t2 -> tout
    ).map { case (a, b) => new Wire(a, b) }



    val ia1 = new ValueLabel("I" -> "1", ra1.t1.current, (xa + 20, 100), symbol = Seq(ValueLabel.currentArrow((xa + 10, 100), Orientation.South)))
    val ia2 = new ValueLabel("I" -> "2", ta2.current, (xa + 20, 300), symbol = Seq(ValueLabel.currentArrow((xa + 10, 300), Orientation.South)))
    val ib1 = new ValueLabel("I" -> "4", rb1.t1.current, (xb + 20, 100), symbol = Seq(ValueLabel.currentArrow((xb + 10, 100), Orientation.South)))
    val ib2 = new ValueLabel("I" -> "5", tb2.current, (xb + 20, 300), symbol = Seq(ValueLabel.currentArrow((xb + 10, 300), Orientation.South)))
    val ic1 = new ValueLabel("I" -> "7", rc1.t1.current, (xc + 20, 100), symbol = Seq(ValueLabel.currentArrow((xc + 10, 100), Orientation.South)))
    val ic2 = new ValueLabel("I" -> "8", tc2.current, (xc + 20, 300), symbol = Seq(ValueLabel.currentArrow((xc + 10, 300), Orientation.South)))

    val iab = new ValueLabel("I" -> "3", wjab.t1current, (xa + 20, 180), symbol = Seq(ValueLabel.currentArrow((xa + 20, 190), Orientation.East)),
      colouringRule = () => wjab.t1current.value match {
        case Some((i, _)) if Math.abs(i - 0.05) < 0.002 => "green"
        case _ => ""
      }
    )
    val ibc = new ValueLabel("I" -> "6", wjbc.t1current, (xb + 20, 180), symbol = Seq(ValueLabel.currentArrow((xb + 20, 190), Orientation.East)))

    val iout = new ValueLabel("I" -> "out", tout.current, (xd, 175), symbol = Seq(ValueLabel.currentArrow((xd, 190), Orientation.East)),
      colouringRule = () => tout.current.value match {
        case Some((i, _)) if Math.abs(i - 0.1) < 0.002 => "green"
        case _ => ""
      }
    )
    val currents = Seq(ia1, ia2, ib1, ib2, ic1, ic2, iout, iab, ibc)

    val i2slider = new ValueSlider(ta2.current, (xa + 20, 320), min = "0", max = "0.1", step = "0.01")(onUpdate = onUpdate, enabled = !this.isComplete )
    val i3slider = new ValueSlider(rb1.t1.current, (xb + 20, 120), min = "0", max = "0.5", step = "0.05")(onUpdate = onUpdate, enabled = !this.isComplete )
    ta1.current.value = Some((-0.1, UserSet))
    tb2.current.value = Some((0.050, UserSet))
    tc1.current.value = Some((-0.050, UserSet))
    tc2.current.value = Some((0.150, UserSet))
    val sliders = Seq(i2slider, i3slider)

    val circuit = new Circuit(Seq(ta1, ta2, tb1, tb2, tc1, tc2, tout, ja, jb, jc, ra1, ra2, rb1, rb2, rc1, rc2, rout) ++ wires ++ currents ++ sliders, 600, 400)

    val propagator = ConstraintPropagator(circuit.components.flatMap(_.constraints))
    propagator.resolve()

    def checkCompletion:Boolean = {
      (for {
        (i, _) <- wjab.t1current.value if Math.abs(i - 0.05) < 0.002
        (j, _) <- tout.current.value if Math.abs(j - 0.1) < 0.002
      } yield true).contains(true)
    }

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

    var completion:Completion = Open

    def render = <.div(Challenge.textAndEx(
      <.div(
        Common.marked(
          """
            |### Something more complex
            |
            |This time, let's give you more than one slider and a slightly more complex circuit fragment.
            |Again, you should just have to consider the currents flowing in and out.
            |
            |Your goal is to set I<sub>3</sub> to 50mA and set I<sub>out</sub> to 100mA
            |
            |""".stripMargin),
        completion match {
          case Complete(_, _) =>
            <.div(
              <.p(
                "Well done. And if you look at each junction in the circuit, the currents in and out should sum to zero."
              ),
              nextButton()
            )
          case _ => <.div()
        }
      )
    )(
      <.div(
        circuit
      )
    ))

  }
}
