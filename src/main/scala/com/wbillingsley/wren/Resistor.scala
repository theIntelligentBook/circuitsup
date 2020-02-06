package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{SVG, ^}
import com.wbillingsley.wren.Orientation.{East, _}


class Resistor(pos:(Int,Int), orientation:Orientation = East, i: Option[Double] = None, r: Option[Double] = None) extends Component {

  def x:Int = pos._1
  def y:Int = pos._2

  val rot = 20

  val current = new Value("A", i.map((_, QuestionSet)))

  val resistance = new Value("Ω", r.map((_, QuestionSet)))

  val voltage = new Value("V")

  val t1 = new Terminal(orientation.rotate((-rot - 1, 0), (0, 0)) + pos)

  val t2 = new Terminal(orientation.rotate((rot + 1, 0), (0, 0)) + pos)

  override def terminals: Seq[Terminal] = Seq(t1, t2)

  override def constraints: Seq[Constraint] = t1.constraints ++ t2.constraints ++ Seq(
    SumConstraint("Kirchhoff's Current Law", Seq(t1.current, t2.current), 0),
    EquationConstraint("Ohm's Law", Seq(
      t1.current -> (() => for { (r, _) <- resistance.value; (v, _) <- voltage.value } yield v / r ),
      voltage -> (() => for { (i, _) <- t1.current.value; (r, _) <- resistance.value } yield i * r ),
      resistance -> (() => for { (i, _) <- t1.current.value; (v, _) <- voltage.value } yield v / i )
    )),
    EquationConstraint("Kirchhoff's Voltage Law", Seq(
      t2.potential -> (() => for { (v1, _) <- t1.potential.value; (v, _) <- voltage.value } yield v1 - v),
      t1.potential -> (() => for { (v2, _) <- t2.potential.value; (v, _) <- voltage.value } yield v2 + v),
      voltage -> (() => for { (v1, _) <- t1.potential.value; (v2, _) <- t2.potential.value } yield v1 - v2),
    ))
  )

  val path = "M -20 0 l 4 0 l 4 -8 l 8 16 l 8 -16 l 8 16 l 4 -8 l 4 0"

  override def render = {

    def icon = SVG.g(^.attr("transform") := s"rotate(${orientation.deg})",
      SVG.path(^.attr("d") := path)
    )

    val (x, y) = pos

    SVG.g(^.cls := "wren-component current-source", ^.attr("transform") := s"translate($x, $y)",
      icon
    )
  }
}