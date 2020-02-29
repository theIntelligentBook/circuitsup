package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{SVG, ^}

class Wire(t1:Terminal, t2:Terminal, via:(Int,Int)*)(implicit colouringRule: Wire.ColouringRule = Wire.defaultColouring) extends Component {

  t1.connect(this)
  t2.connect(this)

  /** The current flowing from t1 to t2 */
  val t1current = new Value("A", None)

  /** The current flowing from t2 to t1 */
  val t2current = new Value("A", None)

  /** The potential of this wire, with respect to the circuit's reference potential */
  val potential = new Value(units = "A", None)

  override def terminals: Seq[Terminal] = Seq.empty

  /** Wires themselves don't have constraints - though the connections they create do */
  override def constraints: Seq[Constraint] = Seq(
    SumConstraint("Kirchoff's Current Law", Seq(t1current, t2current), 0),
    EqualityConstraint("Wires are at constant potential", Seq(t1.potential, potential, t2.potential))
  )

  /** Tells you what the other end of this wire is connected to
   *  TODO: We're assuming that the wire doesn't have both ends connected to the same terminal
   */
  def currentFrom(c:Terminal):Value = if (c == t1) t1current else t2current

  def path = {
    val (x1, y1) = t1.pos
    val (x2, y2) = t2.pos

    s"M $x1 $y1 " +
      via.map({ case (x, y) => s"L $x $y"}).mkString(" ") +
      s" L $x2 $y2"
  }

  override def render = {

    SVG.g(^.cls := s"wren-component wire ${colouringRule(t1current, potential)}",
      SVG.path(^.attr("d") := path)
    )
  }
}

object Wire {

  implicit class Wireable(val pair:(Terminal, Terminal)) extends AnyVal {
    def wire(implicit r:Wire.ColouringRule = Wire.defaultColouring) = new Wire(pair._1, pair._2)(r)

    def wireVia(s:(Int, Int)*)(implicit r:Wire.ColouringRule = Wire.defaultColouring) = new Wire(pair._1, pair._2, s:_*)(r)
  }

  def apply(pairs:(Terminal, Terminal)*):Seq[Wire] = {
    for {
      (t1, t2) <- pairs
    } yield new Wire(t1, t2)
  }

  type ColouringRule = (Value, Value) => String

  val defaultColouring:ColouringRule = (_, _) => "default"

  val voltageColoring:ColouringRule = { (current, voltage) =>
    voltage.value match {
      case Some(v) if v <= NMOSSwitch.vt => "blue"
      case Some(v) if v >= PMOSSwitch.vt => "red"
      case _ => "nocol"
    }

  }


  /*

  wires(r1.t1 -> cs.rt2 via
   */
}