package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{SVG, VHtmlDiffNode, ^}
import com.wbillingsley.wren.Orientation.{East, _}

/**
 * A p-channel enhancement mode MOSFET showing the internals of the device
 * @param pos
 */
class PMOSFETB(pos:(Int, Int), orientation: Orientation = East) extends Component {

  val tWidth = 40
  val tHeight = 40
  val tCW = 5
  val glass = 5

  val gate = new Terminal(orientation.rotate((0, -40)) + pos, Some(0))
  val source = new Terminal(orientation.rotate((75, -40)) + pos)
  val drain = new Terminal(orientation.rotate((-75, -40)) + pos)
  val body = new Terminal(orientation.rotate((0, 40)) + pos, Some(0))

  override def terminals: Seq[Terminal] = Seq(gate, source, drain, body)

  val vt0 = 1.2

  // Taken from an example
  val muCoxWL = 0.0005
  val lambda = 0.05

  override def constraints: Seq[Constraint] = Seq(
    SumConstraint("Kirchhoff's Current Law", Seq(source.current, drain.current), 0),
    EquationConstraint("Source current", drain.current, Seq(body.potential, gate.potential, source.potential, drain.potential), () => for {
        v_gb <- body.potential - gate.potential
        v_ds <- source.potential - drain.potential
      } yield {
        if (v_gb < vt0) 0 else
        if (v_ds < v_gb - vt0) {
          // Linear region
          muCoxWL * ( (v_gb - vt0) * v_ds - Math.pow(v_ds, 2) / 2) * (1 + lambda * v_ds)
        } else {
          // Saturaion region
          muCoxWL * Math.pow(v_gb - vt0, 2) * (1 + lambda * v_ds) / 2
        }
      }
    )
  )

  def channelWidth:Int = {
    val d = for {
      v_gb <- body.potential - gate.potential
    } yield Math.max(0, Math.min(5, v_gb)) * 3
    d.getOrElse(0d).toInt

  }

  override def render = {
    val (x, y) = pos

    SVG.g(^.cls := "wren-component", ^.attr("transform") := s"translate($x, $y) rotate(${orientation.deg})",
      SVG.rect(^.attr("x") := (-100), ^.attr("y") := (-20), ^.attr("width") := 200, ^.attr("height") := 40, ^.cls := "body semiconductor"),
      SVG.rect(^.attr("x") := (-50), ^.attr("y") := (-20), ^.attr("width") := 100, ^.attr("height") := channelWidth, ^.cls := "source semiconductor pdoped channel"),

      SVG.rect(^.attr("x") := (-100), ^.attr("y") := (-20), ^.attr("width") := 50, ^.attr("height") := 20, ^.cls := "source semiconductor pdoped"),
      SVG.rect(^.attr("x") := (50), ^.attr("y") := (-20), ^.attr("width") := 50, ^.attr("height") := 20, ^.cls := "drain semiconductor pdoped"),
      SVG.rect(^.attr("x") := (-50), ^.attr("y") := (-30), ^.attr("width") := 100, ^.attr("height") := 10, ^.cls := "semiconductor glass"),
      SVG.path(^.cls := "wire", ^.attr("d") := "M -75 -40 l 0 20"),
      SVG.path(^.cls := "wire", ^.attr("d") := "M 75 -40 l 0 20"),
      SVG.path(^.cls := "wire", ^.attr("d") := "M 0 -40 l 0 10"),
      SVG.path(^.cls := "wire", ^.attr("d") := "M 0 20 l 0 20"),
      SVG.path(^.cls := "semiconductor-connector", ^.attr("d") := "M -100 -20 l 50 0"),
      SVG.path(^.cls := "semiconductor-connector", ^.attr("d") := "M 50 -20 l 50 0"),
      SVG.path(^.cls := "semiconductor-connector", ^.attr("d") := "M -50 -30 l 100 0"),
      SVG.path(^.cls := "semiconductor-connector", ^.attr("d") := "M -50 20 l 100 0")
    )


  }
}
