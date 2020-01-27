package com.wbillingsley.wren
import com.wbillingsley.veautiful.html.{SVG, VHtmlDiffNode, ^}

case class Ground(pos:(Int,Int)) extends Component {

  val terminal = new Terminal(pos)
  terminal.current.value = Some(0d -> UserSet)
  terminal.potential.value = Some(0d -> UserSet)

  override def terminals: Seq[Terminal] = Seq(terminal)

  override def constraints: Seq[Constraint] = Seq.empty

  override def render: VHtmlDiffNode = {
    val (x, y) = pos
    SVG.g(^.cls := "wren-component ground", ^.attr("transform") := s"translate($x, $y)",
      SVG.path(^.attr("d") := "M 0 0 l 0 10 l -5 0 l 5 5 l 5 -5 l -5 0")
    )
  }
}
