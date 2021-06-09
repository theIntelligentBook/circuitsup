package com.wbillingsley.wren

import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, SVG, VHtmlComponent, ^}
import com.wbillingsley.veautiful.logging.Logger
import org.scalajs.dom.{Element, Node}

import scala.collection.mutable

case class LogicPlot(values:Seq[Value], names:Map[Int, String] = Map.empty, lineHeight:Int = 50, width:Int = 300, seriesLength:Int = 100) extends VHtmlComponent {

  private val logger = Logger.getLogger(getClass)

  private val buffers = values.indices.map(_ => new mutable.ArrayDeque[Double](seriesLength))

  lazy val offset = 2 // padding
  lazy val range = lineHeight - 2 * offset // padding

  val vcc = 5d

  def updateValues():Unit = {
    for { i <- values.indices } {
      val b = buffers(i)
      val v = values(i).value.getOrElse(vcc / 2)
      if (b.length >= seriesLength) b.removeHead()
      b.append(v)
    }
    rerender()
  }

  private def y(d:Double):Int = {
    val p = 1 - (d / vcc)
    (p * range + offset).toInt
  }

  private def x(i:Int):Int = {
    val p = (i.toDouble/ seriesLength)
    (p * width).toInt
  }

  private def calcPath(i:Int):String = {
    val series = buffers(i)
    if (series.isEmpty) "" else {
      s"M 0 ${y(series.head)} " + (
        for { i <- series.indices } yield {
          s"L ${x(i)} ${y(series(i))} "
        }
      ).mkString
    }
  }

  private def name(i:Int):String = {
    names.getOrElse(i, values(i).name.getOrElse("Unnamed"))
  }

  override protected def render: DiffNode[Element, Node] = {
    <.div(^.cls := "logic-plot",
      for { i <- values.indices } yield {
        <.div(^.cls := "logic-row",
          <.span(^.cls := "name", name(i)),
          <.span(
            <.svg(^.cls := "logic-line", ^.attr("width") := width, ^.attr("height") := lineHeight,
              SVG.path(^.attr("d") := calcPath(i))
            )
          )
        )
      }
    )
  }

}
