package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{SVG, VHtmlDiffNode, ^}
import com.wbillingsley.wren.Orientation.East

class LogicProbe(pos:(Int,Int), orientation:Orientation = East, name:String="")(implicit colouringRule: Wire.ColouringRule = Wire.defaultColouring) extends Component {

  val t = new Terminal(pos, Some(0))

  override def terminals: Seq[Terminal] = Seq(t)

  override def constraints: Seq[Constraint] = t.constraints

  def value:Option[Boolean] = t.potential.value.flatMap {
    case v if v >= LogicProbe.min1 => Some(true)
    case v if v <= LogicProbe.max0 => Some(false)
    case _ => None
  }

  override def render = {
    val (x, y) = pos
    val wireCol = colouringRule(t.current, t.potential)

    SVG.g(^.cls := "wren-component logic-probe", ^.attr("transform") := s"translate($x, $y) rotate(${orientation.deg})",
      SVG.path(^.cls := wireCol, ^.attr("d") := "M 0 0 l 10 0"),
      SVG.rect(^.attr("x") := "10", ^.attr("y") := "-20", ^.attr("width") := 40, ^.attr("height") := 40),
      SVG.text(^.cls := "display", ^.attr("x") := 30, ^.attr("y") := 0,
        if (t.potential >= LogicProbe.min1) "1"
        else if (t.potential<= LogicProbe.max0) "0"
        else "?"
      ),
      SVG.text(^.cls := "output-name", ^.attr("x") := "55", ^.attr("y") := 0, name)
    )
  }
}

object LogicProbe {

  val min1:Double = 3.5

  val max0:Double = 1.5

  val vdd:Double = 5

  val vss:Double = 0

}