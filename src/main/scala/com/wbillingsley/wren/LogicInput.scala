package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{<, SVG, VHtmlDiffNode, ^}
import com.wbillingsley.wren.Orientation.East

class LogicInput(pos:(Int,Int), orientation:Orientation = East, name:String="", initial:Option[Boolean] = None)(onUpdate: Option[Boolean] => Unit)(implicit colouringRule: Wire.ColouringRule = Wire.defaultColouring) extends Component {

  val t = new Terminal(pos, Some(0))

  override def terminals: Seq[Terminal] = Seq(t)

  override def constraints: Seq[Constraint] = t.constraints

  private var _value:Option[Boolean] = initial
  updatePotential()

  def value:Option[Boolean] = _value
  def value_=(o:Option[Boolean]) = {
    _value = o
    updatePotential()
  }

  def updatePotential():Unit = {
    t.potential.content = for { v <- value } yield ( if (v) LogicProbe.vdd else LogicProbe.vss, UserSet )
  }

  private def click():Unit = {
    value = Some(value.contains(false))
    onUpdate(value)
  }

  override def render: VHtmlDiffNode = {
    val (x, y) = pos
    val wireCol = colouringRule(t.current, t.potential)

    SVG.g(^.cls := "wren-component logic-input", ^.attr("transform") := s"translate($x, $y) rotate(${orientation.deg})",
      SVG.foreignObject(^.attr("width") := "30", ^.attr("height") := "40", ^.attr("x") := "-40", ^.attr("y") := "-20",
        <.button(^.cls := "logic-input", ^.onClick --> click(),
          value match {
            case Some(true) => "1"
            case Some(false) => "0"
            case None => "-"
          }
        )
      ),
      SVG.path(^.cls := wireCol, ^.attr("d") := "M 0 0 l -10 0"),
      SVG.text(^.cls := "input-name", ^.attr("x") := "-50", ^.attr("y") := 0, name)
    )
  }
}

