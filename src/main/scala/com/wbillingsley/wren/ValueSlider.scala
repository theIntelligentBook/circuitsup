package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{<, SVG, VHtmlDiffNode, ^}
import org.scalajs.dom.Event
import org.scalajs.dom.raw.HTMLInputElement

import scala.util.Random

class ValueSlider(v:Value, pos:(Int, Int), orientation: Orientation = Orientation.East, min:String = "1", max:String, step:String = "1", values:Seq[String] = Seq.empty)
                 (onUpdate: () => Unit, enabled: => Boolean = true) extends Component {

  val id = Random.nextString(5)

  override def terminals: Seq[Terminal] = Seq.empty

  override def constraints: Seq[Constraint] = Seq.empty

  val updateValue: Event => Unit = (e:Event) => {
    v.content = e.target match {
      case i:HTMLInputElement => Some(i.value.toDouble, UserSet)
      case _ => None
    }
    onUpdate()
  }

  def slider = <.input(^.cls := "value-slider",
    ^.attr("type") := "range",
    ^.attr("min") := min,
    ^.attr("max") := max,
    ^.attr("step") := step,
    ^.attr("disabled") ?= (if (enabled) None else Some("disabled")),
    ^.attr("list") := id,
    ^.on("input") ==> updateValue
  )

  override def render = {
    val (x, y) = pos

    SVG.foreignObject(^.attr("x") := x, ^.attr("y") := y, ^.attr("width") := 150, ^.attr("height") := 30,
      slider,
      <("datalist")(^.attr("id") := id,
        values.map { v => <("option")(^.attr("value") := v)}
      )
    )
  }

}
