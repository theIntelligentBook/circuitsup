package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{SVG, VHtmlDiffNode, ^}
import com.wbillingsley.wren.Orientation.East

case class HalfAdder(pos:(Int,Int), orientation:Orientation = East)(implicit colouringRule: Wire.ColouringRule = Wire.defaultColouring) extends Component {

  val (x, y) = pos

  val ta = new Terminal((x - 70, y + 20), Some(0))
  val tb = new Terminal((x - 70, y + 40), Some(0))
  val tr = new Terminal((x + 70, y + 40), Some(0))
  val tc = new Terminal((x + 70, y + 20), Some(0))

  override def terminals: Seq[Terminal] = Seq(ta, tb, tc, tr)

  override def constraints: Seq[Constraint] = terminals.flatMap(_.constraints) :+
    EquationConstraint("NOT gate", Seq(
      tc.potential -> (() => for { cc <- c } yield if (cc) LogicProbe.vdd else LogicProbe.vss),
      tr.potential -> (() => for { rr <- r } yield if (rr) LogicProbe.vdd else LogicProbe.vss)
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

  def c:Option[Boolean] = for { aa <- a; bb <- b } yield aa && bb
  def r:Option[Boolean] = for { aa <- a; bb <- b } yield aa ^ bb

  override def render: VHtmlDiffNode = {
    val (x, y) = pos

    SVG.g(^.cls := "wren-component half-adder", ^.attr("transform") := s"translate($x, $y) rotate(${orientation.deg})",
      SVG.rect(^.attr("x") := "-60", ^.attr("y") := "-40", ^.attr("width") := "120", ^.attr("height") := "100"),
      SVG.path(^.cls := colouringRule(ta.current, ta.potential), ^.attr("d") := "M -60 20 l -10 0"),
      SVG.path(^.cls := colouringRule(tb.current, tb.potential), ^.attr("d") := "M -60 40 l -10 0"),
      SVG.path(^.cls := colouringRule(tr.current, tr.potential), ^.attr("d") := "M 60 40 l 10 0"),
      SVG.path(^.cls := colouringRule(tc.current, tc.potential), ^.attr("d") := "M 60 20 l 10 0"),
      SVG.text(^.cls := "name", ^.attr("x") := "0", ^.attr("y") := "-20", "Half-Adder"),
      SVG.text(^.cls := "input", ^.attr("x") := "-55", ^.attr("y") := "20", "A"),
      SVG.text(^.cls := "input", ^.attr("x") := "-55", ^.attr("y") := "40", "B"),
      SVG.text(^.cls := "output", ^.attr("x") := "55", ^.attr("y") := "40", "Result"),
      SVG.text(^.cls := "output", ^.attr("x") := "55", ^.attr("y") := "20", "Carry"),
    )
  }
}