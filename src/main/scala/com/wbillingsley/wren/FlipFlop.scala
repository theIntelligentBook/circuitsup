package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{SVG, VHtmlDiffNode, ^}
import com.wbillingsley.wren.Orientation.East

case class FlipFlop(pos:(Int,Int), orientation:Orientation = East)(implicit colouringRule: Wire.ColouringRule = Wire.defaultColouring) extends Component {

  val (x, y) = pos

  val td = new Terminal((x - 40, y - 20), Some(0))
  val clk = new Terminal((x - 40, y + 20), Some(0))

  val q = new Terminal((x + 40, y - 20), Some(0))
  val qbar = new Terminal((x + 40, y + 20), Some(0))

  override def terminals: Seq[Terminal] = Seq(td, clk, qbar, q)

  def inputs = Seq(td.potential, clk.potential)

  var lastClock: Option[Boolean] = None

  var value: Option[Boolean] = None

  override def constraints: Seq[Constraint] = terminals.flatMap(_.constraints) ++ Seq(
    EquationConstraint("Flip flop Q", q.potential, Seq(td.potential, clk.potential), () =>
      updateValue().map(if (_) LogicProbe.vdd else LogicProbe.vss)
    ),
    EquationConstraint("Flip flop Qbar", qbar.potential, Seq(q.potential), () =>
      value.map(if (_) LogicProbe.vss else LogicProbe.vdd)
    ),
  )

  def d:Option[Boolean] = td.potential.value.flatMap {
    case v if v >= LogicProbe.min1 => Some(true)
    case v if v <= LogicProbe.max0 => Some(false)
    case _ => None
  }

  def clock:Option[Boolean] = clk.potential.value.flatMap {
    case v if v >= LogicProbe.min1 => Some(true)
    case v if v <= LogicProbe.max0 => Some(false)
    case _ => None
  }

  /**
   * Updates the flip-flop's internal value. Note that this requires an instantaneous 0 -> 1 transition on the clock
   * @return
   */
  def updateValue():Option[Boolean] = {
    val trigger:Boolean = (
      (for {
        c <- clock if c
        last <- lastClock if !last
      } yield true).getOrElse(false)
    )
    lastClock = clock
    if (trigger) value = d
    value
  }

  override def render = {
    val (x, y) = pos

    SVG.g(^.cls := "wren-component flip-flop", ^.attr("transform") := s"translate($x, $y) rotate(${orientation.deg})",
      SVG.rect(^.attr("x") := "-30", ^.attr("y") := "-40", ^.attr("width") := "60", ^.attr("height") := "80"),
      SVG.path(^.cls := colouringRule(td.current, td.potential), ^.attr("d") := "M -30 -20 l -10 0"),
      SVG.path(^.cls := colouringRule(clk.current, clk.potential), ^.attr("d") := "M -30 20 l -10 0"),
      SVG.path(^.cls := colouringRule(q.current, q.potential), ^.attr("d") := "M 30 -20 l 10 0"),
      SVG.path(^.cls := colouringRule(qbar.current, qbar.potential), ^.attr("d") := "M 30 20 l 10 0"),
      SVG.text(^.cls := "input", ^.attr("x") := "-25", ^.attr("y") := "-20", "D"),
      SVG.path(^.attr("d") := "M -30 15 l 10 5 l -10 5"),
      SVG.text(^.cls := "output", ^.attr("x") := "25", ^.attr("y") := "-20", "Q"),
      SVG.text(^.cls := "output", ^.attr("text-decoration"):= "overline", ^.attr("x") := "25", ^.attr("y") := "20", "Q"),
    )
  }
}