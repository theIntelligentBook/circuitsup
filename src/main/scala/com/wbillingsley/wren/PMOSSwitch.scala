package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{SVG, VHtmlDiffNode, ^}
import com.wbillingsley.wren.Orientation.{East, _}

/**
 * A simplified model of an n-MOSFET, that just acts as a switch if the gate voltage is above threshold
 * @param pos
 */
class PMOSSwitch(pos:(Int, Int), orientation: Orientation = East) extends Component {
  def x:Int = pos._1
  def y:Int = pos._2

  val gate = new Terminal(orientation.rotate((-50, 0)) + pos, Some(0))
  val source = new Terminal(orientation.rotate((0, -40)) + pos)
  val drain = new Terminal(orientation.rotate((0, 40)) + pos)

  override def terminals: Seq[Terminal] = Seq(gate, source, drain)

  val vt = 5 - 1.2

  override def constraints: Seq[Constraint] = Seq(
    SumConstraint("Kirchhoff's Current Law", Seq(source.current, drain.current), 0),
    EquationConstraint("nMOSFET is on", Seq(
      drain.potential -> (() => for {
        (vg, _) <- gate.potential.value if vg <= vt
        (p, _) <- source.potential.value
      } yield p),
      source.potential -> (() => for {
        (vg, _) <- gate.potential.value if vg <= vt
        (p, _) <- drain.potential.value
      } yield p)
    )),
    EquationConstraint("nMOSFET is off", Seq(
      source.current -> (() => for {
        (vg, _) <- gate.potential.value if vg > vt
      } yield 0d))
    )
  ) ++ terminals.flatMap(_.constraints)

  override def render: VHtmlDiffNode = {
    val (x, y) = pos

    SVG.g(^.cls := "wren-component", ^.attr("transform") := s"translate($x, $y) rotate(${orientation.deg})",
      SVG.path(^.attr("d") := "M 0 -40 l 0 20 l -20 0 l 0 40 l 20 0 l 0 20"),
      SVG.path(^.attr("d") := "M -30 -20 l 0 40"),
      SVG.circle(^.attr("cx") := "-35", ^.attr("cy") := "0", ^.attr("r") := "5"),
      SVG.path(^.attr("d") := "M -50 0 l 10 0"),
    )


  }
}
