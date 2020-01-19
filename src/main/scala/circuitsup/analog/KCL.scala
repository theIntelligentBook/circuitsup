package circuitsup.analog

import circuitsup.{CircuitsRoute, Router}
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.html.{<, ^}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Completion, Open}
import com.wbillingsley.wren.Orientation.South
import com.wbillingsley.wren.{Circuit, ConstraintPropagator, CurrentSource, Orientation, Resistor, Terminal, ValueLabel, ValueSlider, VoltageSource, Wire}

object KCL {

  val vd = new VoltageDivider()

  object page1 extends ExerciseStage {

    val junction = new OneWireFragment((100, 100), 200)
    val i1 = new ValueLabel("I" -> "1", junction.wire.t1current, (120, 100), symbol = Seq(ValueLabel.currentArrow((110, 100), Orientation.South)))
    val i2 = new ValueLabel("I" -> "2", junction.t2.current, (120, 300), symbol = Seq(ValueLabel.currentArrow((110, 300), Orientation.South)))
    val i1slider = new ValueSlider(junction.wire.t1current, (120, 120), min = "0", max = "1", step = "0.01")(onUpdate = onUpdate, enabled = !this.isComplete )

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
      }

      Router.rerender()
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
              <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(CircuitsRoute(1, 1)), s"Next")
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
    val i2 = new ValueLabel("I" -> "2", junction.wire.t2current, (120, 300), symbol = Seq(ValueLabel.currentArrow((110, 300), Orientation.North)))
    val i1slider = new ValueSlider(junction.wire.t1current, (120, 120), min = "0", max = "1", step = "0.01")(onUpdate = onUpdate, enabled = !this.isComplete )

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
      }

      Router.rerender()
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
              <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(CircuitsRoute(1, 2)), s"Next")
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
    val i2 = new ValueLabel("I" -> "2", r1.t1.current, (120, 300), symbol = Seq(ValueLabel.currentArrow((110, 300), Orientation.South)))
    val i1slider = new ValueSlider(t2.current, (120, 120), min = "0", max = "1", step = "0.01")(onUpdate = onUpdate, enabled = !this.isComplete )

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
      }

      Router.rerender()
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
              <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(CircuitsRoute(1, 2)), s"Next")
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
