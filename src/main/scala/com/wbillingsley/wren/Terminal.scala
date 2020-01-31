package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{SVG, VHtmlDiffNode, ^}

import scala.collection.mutable

sealed trait Connector {
  val wires: mutable.ArrayBuffer[Wire] = mutable.ArrayBuffer.empty

  def connect(w:Wire):Unit = {
    wires.append(w)
  }

  def pos:(Int, Int)
}

class Terminal(val pos:(Int, Int), i: Option[Double] = None) extends Connector with Component {

  /** The current into this terminal */
  val current = new Value("A", i.map((_, QuestionSet)))

  /** The potential of this terminal, with respect to the circuit's reference potential */
  val potential = new Value(units = "A", None)

  override def terminals: Seq[Terminal] = Seq.empty

  override def constraints: Seq[Constraint] = Seq(
    SumConstraint("Kirchhoff's Current Law", wires.map(_.currentFrom(this)).append(current).toSeq, 0),
    EqualityConstraint("Connections are at constant potential", wires.map(_.potential).toSeq)
  )

  override def render = {
    val (x, y) = pos

    SVG.circle(^.attr("cx") := x, ^.attr("cy") := y, ^.cls := "terminal")
  }
}

/**
 * A junction is a terminal with no inbound current
 */
case class Junction(pos:(Int, Int)) extends Connector with Component {

  val terminal = new Terminal(pos)

  override def terminals: Seq[Terminal] = Seq(
    terminal
  )

  override def constraints: Seq[Constraint] = {
    EquationConstraint("Junction's terminal current is zero", Seq(terminal.current -> (() => Some(0)))) +: terminal.constraints
  }


  override def render: VHtmlDiffNode = {
    val (x, y) = pos

    SVG.circle(^.attr("cx") := x, ^.attr("cy") := y, ^.cls := "junction")
  }
}

