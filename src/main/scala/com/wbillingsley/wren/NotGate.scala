package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{SVG, VHtmlDiffNode, ^}
import com.wbillingsley.wren.Orientation.East

class NotGate(pos:(Int,Int), orientation:Orientation = East)(implicit colouringRule: Wire.ColouringRule = Wire.defaultColouring) extends Component {

  val (x, y) = pos

  val in = new Terminal((x - 30, y), Some(0))
  val out = new Terminal((x + 30, y), Some(0))

  override def terminals: Seq[Terminal] = Seq(in, out)

  override def constraints: Seq[Constraint] = in.constraints ++ out.constraints :+ EquationConstraint("NOT gate",
    out.potential, Seq(in.potential), () => for { v <- value } yield if (v) LogicProbe.vdd else LogicProbe.vss
  )

  def a:Option[Boolean] = in.potential.value.flatMap {
    case v if v >= LogicProbe.min1 => Some(true)
    case v if v <= LogicProbe.max0 => Some(false)
    case _ => None
  }

  def value:Option[Boolean] = for { aa <- a } yield !aa

  override def render = {
    val (x, y) = pos

    SVG.g(^.cls := "wren-component logic-probe", ^.attr("transform") := s"translate($x, $y) rotate(${orientation.deg})",
      SVG.path(^.cls := colouringRule(in.current, in.potential), ^.attr("d") := "M -20 0 l -10 0"),
      SVG.path(^.cls := colouringRule(out.current, out.potential), ^.attr("d") := "M 20 0 l 10 0"),
      SVG.path(^.attr("d") := "M 10 0 l -30 -20 l 0 40 l 30 -20"),
      SVG.circle(^.attr("cx") := "15", ^.attr("cy") := "0", ^.attr("r") := "5")
    )
  }
}
