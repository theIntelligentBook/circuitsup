package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{<, SVG, VHtmlDiffNode, ^}
import com.wbillingsley.wren.Orientation.East

class LogicInput(pos:(Int,Int), orientation:Orientation = East)(onUpdate: () => Unit)(implicit colouringRule: Wire.ColouringRule = Wire.defaultColouring) extends Component {

  val t = new Terminal(pos, Some(0))

  override def terminals: Seq[Terminal] = Seq(t)

  override def constraints: Seq[Constraint] = t.constraints

  var value:Option[Boolean] = None

  def updatePotential():Unit = {
    t.potential.content = for { v <- value } yield ( if (v) LogicProbe.vdd else LogicProbe.vss, UserSet )
  }

  def toggle():Unit = {
    value = Some(value.contains(false))
    updatePotential()
    onUpdate()
  }

  override def render: VHtmlDiffNode = {
    val (x, y) = pos
    val wireCol = colouringRule(t.current, t.potential)

    SVG.g(^.cls := "wren-component logic-input", ^.attr("transform") := s"translate($x, $y) rotate(${orientation.deg})",
      SVG.foreignObject(^.attr("width") := "30", ^.attr("height") := "40", ^.attr("x") := "-40", ^.attr("y") := "-20",
        <.button(^.cls := "logic-input", ^.onClick --> toggle(),
          value match {
            case Some(true) => "1"
            case Some(false) => "0"
            case None => "-"
          }
        )
      ),
      SVG.path(^.cls := wireCol, ^.attr("d") := "M 0 0 l -10 0"),
    )
  }
}

