package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{SVG, VHtmlDiffNode, ^}
import com.wbillingsley.wren.Orientation.East

case class FullAdder(pos:(Int,Int), orientation:Orientation = East)(implicit colouringRule: Wire.ColouringRule = Wire.defaultColouring) extends Component {

  val (x, y) = pos

  val tcin = new Terminal((x - 70, y + 45), Some(0))
  val ta = new Terminal((x - 70, y + 5), Some(0))
  val tb = new Terminal((x - 70, y + 25), Some(0))
  val tr = new Terminal((x + 70, y + 45), Some(0))
  val tcout = new Terminal((x + 70, y + 25), Some(0))

  override def terminals: Seq[Terminal] = Seq(tcin, ta, tb, tcout, tr)

  def inputs = Seq(ta.potential, tb.potential, tcin.potential)

  override def constraints: Seq[Constraint] = terminals.flatMap(_.constraints) :+
    EquationConstraint("Full Adder carry out", tcout.potential, inputs, () => for {cc <- c} yield if (cc) LogicProbe.vdd else LogicProbe.vss) :+
    EquationConstraint("Full Adder result", tr.potential, inputs, () => for { rr <- r } yield if (rr) LogicProbe.vdd else LogicProbe.vss)

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

  def cin:Option[Boolean] = tcin.potential.value.flatMap {
    case v if v >= LogicProbe.min1 => Some(true)
    case v if v <= LogicProbe.max0 => Some(false)
    case _ => None
  }

  def c:Option[Boolean] = for { aa <- a; bb <- b; cc <- cin } yield (aa && bb) || (cc && (aa ^ bb))
  def r:Option[Boolean] = for { aa <- a; bb <- b; cc <- cin } yield (aa ^ bb ^ cc)

  override def render: VHtmlDiffNode = {
    val (x, y) = pos

    SVG.g(^.cls := "wren-component half-adder", ^.attr("transform") := s"translate($x, $y) rotate(${orientation.deg})",
      SVG.rect(^.attr("x") := "-60", ^.attr("y") := "-40", ^.attr("width") := "120", ^.attr("height") := "100"),
      SVG.path(^.cls := colouringRule(tcin.current, tcin.potential), ^.attr("d") := "M -60 45 l -10 0"),
      SVG.path(^.cls := colouringRule(ta.current, ta.potential), ^.attr("d") := "M -60 5 l -10 0"),
      SVG.path(^.cls := colouringRule(tb.current, tb.potential), ^.attr("d") := "M -60 25 l -10 0"),
      SVG.path(^.cls := colouringRule(tr.current, tr.potential), ^.attr("d") := "M 60 45 l 10 0"),
      SVG.path(^.cls := colouringRule(tcout.current, tcout.potential), ^.attr("d") := "M 60 25 l 10 0"),
      SVG.text(^.cls := "name", ^.attr("x") := "0", ^.attr("y") := "-20", "Full-Adder"),
      SVG.text(^.cls := "input", ^.attr("x") := "-55", ^.attr("y") := "45", "Cin"),
      SVG.text(^.cls := "input", ^.attr("x") := "-55", ^.attr("y") := "5", "A"),
      SVG.text(^.cls := "input", ^.attr("x") := "-55", ^.attr("y") := "25", "B"),
      SVG.text(^.cls := "output", ^.attr("x") := "55", ^.attr("y") := "45", "Result"),
      SVG.text(^.cls := "output", ^.attr("x") := "55", ^.attr("y") := "25", "Carry"),
    )
  }
}