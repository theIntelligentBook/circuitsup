package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{SVG, VHtmlDiffNode, ^}
import com.wbillingsley.wren.Orientation.East

class XorGate(pos:(Int,Int), orientation:Orientation = East)(implicit colouringRule: Wire.ColouringRule = Wire.defaultColouring) extends Component {

  val (x, y) = pos

  val ta = new Terminal((x - 30, y - 10), Some(0))
  val tb = new Terminal((x - 30, y + 10), Some(0))
  val out = new Terminal((x + 35, y), Some(0))

  override def terminals: Seq[Terminal] = Seq(ta, tb, out)

  override def constraints: Seq[Constraint] = ta.constraints ++ tb.constraints ++ out.constraints :+ EquationConstraint("NOT gate", Seq(
    out.potential -> (() => for { v <- value } yield if (v) LogicProbe.vdd else LogicProbe.vss)
  ))

  def a:Option[Boolean] = ta.potential.value.flatMap {
    case v if v >= LogicProbe.min1 => Some(true)
    case v if v <= LogicProbe.max0 => Some(false)
    case _ => None
  }

  def b:Option[Boolean] = tb.potential.value.flatMap {
    case v if v >= LogicProbe.min1 => Some(true)
    case v if v <= LogicProbe.max0 => Some(false)
    case _ => None
  }

  def value:Option[Boolean] = for { aa <- a; bb <- b } yield aa != bb

  override def render: VHtmlDiffNode = {
    val (x, y) = pos

    SVG.g(^.cls := "wren-component logic-probe", ^.attr("transform") := s"translate($x, $y) rotate(${orientation.deg})",
      SVG.path(^.cls := colouringRule(ta.current, ta.potential), ^.attr("d") := "M -10 -10 l -20 0"),
      SVG.path(^.cls := colouringRule(tb.current, tb.potential), ^.attr("d") := "M -10 10 l -20 0"),
      SVG.path(^.cls := colouringRule(out.current, out.potential), ^.attr("d") := "M 25 0 l 10 0"),
      SVG.path(^.attr("d") := "M -15 -20 c 30 0 40 20 40 20 c 0 0 -10 20 -40 20 c 10 -20 10 -20 0 -40"),
      SVG.path(^.attr("d") := "M -20 20 c 10 -20 10 -20 0 -40"),
    )
  }
}
