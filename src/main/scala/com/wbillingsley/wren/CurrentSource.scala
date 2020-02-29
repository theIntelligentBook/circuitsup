package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{SVG, ^}
import com.wbillingsley.wren.Orientation._
import com.wbillingsley.wren.ValueLabel.arrow

class CurrentSource(pos:(Int,Int), orientation:Orientation = East, initial: Option[Double] = None) extends Component {

  val r = 20

  val current = new Value("A", initial.map((_, QuestionSet)))

  val voltage = new Value("V")

  val t1 = new Terminal(orientation.rotate((-r - 1, 0), (0, 0)) + pos)

  val t2 = new Terminal(orientation.rotate((r + 1, 0), (0, 0)) + pos)

  override def terminals: Seq[Terminal] = Seq(t1, t2)

  override def constraints: Seq[Constraint] = Seq(
    EqualityConstraint("Current source", Seq(t1.current, current)),
    SumConstraint("Kirchhoff's Current Law", Seq(t1.current, t2.current), 0),
    EquationConstraint("Kirchhoff's Voltage Law", Seq(
      t2.potential -> (() => for {(v1, _) <- t1.potential.content; (v, _) <- voltage.content} yield v1 + v),
      t1.potential -> (() => for {(v2, _) <- t2.potential.content; (v, _) <- voltage.content} yield v2 - v),
      voltage -> (() => for {(v1, _) <- t1.potential.content; (v2, _) <- t2.potential.content} yield v2 - v1),
    ))
  ) ++ t1.constraints ++ t2.constraints

  override def render = {

    def icon = SVG.g(^.attr("transform") := s"rotate(${orientation.deg})",
      SVG.circle(^.attr("cx") := 0, ^.attr("cy") := 0, ^.attr("r") := r),
      SVG.path(^.attr("d") := "M -8 0 l 16 0 l -6 -8 m 6 8 l -6 8")
    )

    val (x, y) = pos

    SVG.g(^.cls := "wren-component current-source", ^.attr("transform") := s"translate($x, $y)",
      icon
    )
  }
}
