package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{SVG, VHtmlDiffNode, ^}
import com.wbillingsley.wren.Orientation.East

class LogicProbe(pos:(Int,Int), orientation:Orientation = East) extends Component {

  val t = new Terminal(pos, Some(0))

  override def terminals: Seq[Terminal] = Seq(t)

  override def constraints: Seq[Constraint] = t.constraints

  override def render: VHtmlDiffNode = {
    val (x, y) = pos
    SVG.g(^.cls := "wren-component logic-probe", ^.attr("transform") := s"translate($x, $y) rotate(${orientation.deg})",
      SVG.path(^.attr("d") := "M 0 0 l 10 0"),
      SVG.rect(^.attr("x") := "10", ^.attr("y") := "-20", ^.attr("width") := 40, ^.attr("height") := 40),
      SVG.text(^.attr("x") := 30, ^.attr("y") := 0,
        if (t.potential >= 3.5) "1"
        else if (t.potential<= 1.5) "0"
        else "?"
      )
    )
  }
}
