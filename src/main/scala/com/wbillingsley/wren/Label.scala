package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{SVG, VHtmlDiffNode, ^}
import com.wbillingsley.wren.Component.ColouringRule

class Label(text:String, pos:(Int, Int), cssClass:String = "", colouringRule:ColouringRule = () => "") extends Component {

  def render: VHtmlDiffNode = {
    val (x, y) = pos

    SVG.g(^.cls := "wren-component label " + colouringRule(),
      SVG.text(^.attr("x") := x, ^.attr("y") := y, ^.cls := cssClass,
        text
      )
    )
  }

  override def terminals: Seq[Terminal] = Seq.empty

  override def constraints: Seq[Constraint] = Seq.empty
}