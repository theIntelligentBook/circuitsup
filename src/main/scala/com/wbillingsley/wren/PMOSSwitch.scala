package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{SVG, VHtmlDiffNode, ^}
import com.wbillingsley.wren.Orientation.{East, _}

/**
 * A simplified model of an n-MOSFET, that just acts as a switch if the gate voltage is above threshold
 * @param pos
 */
class PMOSSwitch(pos:(Int, Int), orientation: Orientation = East)(implicit val colouringRule:PMOSSwitch.ColouringRule = PMOSSwitch.defaultColouring) extends Component {
  def x:Int = pos._1
  def y:Int = pos._2

  val gate = new Terminal(orientation.rotate((-50, 0)) + pos, Some(0))
  val source = new Terminal(orientation.rotate((0, -40)) + pos)
  val drain = new Terminal(orientation.rotate((0, 40)) + pos)

  override def terminals: Seq[Terminal] = Seq(gate, source, drain)

  val vt = PMOSSwitch.vt

  def on:Boolean = gate.potential < vt

  override def constraints: Seq[Constraint] = Seq(
    SumConstraint("Kirchhoff's Current Law", Seq(source.current, drain.current), 0),
    EquationConstraint("nMOSFET is on", drain.potential, Seq(gate.potential, source.potential), () => for {
        (vg, _) <- gate.potential.content if vg <= vt
        (p, _) <- source.potential.content
    } yield p),
    EquationConstraint("nMOSFET is on", source.potential, Seq(gate.potential, drain.potential), () => for {
        (vg, _) <- gate.potential.content if vg <= vt
        (p, _) <- drain.potential.content
    } yield p),
    EquationConstraint("nMOSFET is off", source.current, Seq(gate.potential), () => for {
        (vg, _) <- gate.potential.content if vg > vt
      } yield 0d)
  ) ++ terminals.flatMap(_.constraints)

  override def render = {
    val (x, y) = pos

    val gateColour = colouringRule.gateRule(this)
    val sdColour = colouringRule.sdRule(this)

    SVG.g(^.cls := "wren-component pmos-switch", ^.attr("transform") := s"translate($x, $y) rotate(${orientation.deg})",
      SVG.path(^.cls := sdColour, ^.attr("d") := "M 0 -40 l 0 20 l -20 0 l 0 40 l 20 0 l 0 20"),
      SVG.path(^.cls := gateColour, ^.attr("d") := "M -30 -20 l 0 40"),
      SVG.circle(^.cls := gateColour, ^.attr("cx") := "-35", ^.attr("cy") := "0", ^.attr("r") := "5"),
      SVG.path(^.cls := gateColour, ^.attr("d") := "M -50 0 l 10 0"),
    )


  }
}

object PMOSSwitch {

  val vt = 5 - 1.2

  class ColouringRule(val gateRule: PMOSSwitch => String, val sdRule: PMOSSwitch => String)

  val defaultColouring = new ColouringRule(_ => "default", _ => "default")

  val voltageColouring = new ColouringRule(
    gateRule = { n => Wire.voltageColoring(n.gate.current, n.gate.potential) },
    sdRule = { n => if (n.on) Wire.voltageColoring(n.drain.current, n.drain.potential) else " off" }
  )

}