package com.wbillingsley.wren

import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, VHtmlComponent, ^}
import com.wbillingsley.veautiful.logging.Logger
import org.scalajs.dom
import org.scalajs.dom.{Element, Node}

case class AnimationPlayer(timeout:Int, initial:Boolean = false)(onTick: Double => Unit) extends VHtmlComponent {

  private val logger = Logger.getLogger(getClass)

  private var playing = initial

  private var last:Double = 0d

  override def afterAttach(): Unit = {
    logger.debug("Animation player attached")
    super.afterAttach()
    conditionalRepeat()
  }

  override def beforeDetach(): Unit = {
    logger.debug("Animation player detached")
    playing = false
    super.beforeDetach()
  }

  def tickFunc(d:Double):Unit = {
    if (d - last > timeout) {
      last = d
      onTick(d)
    }
    conditionalRepeat()
  }

  def conditionalRepeat():Unit = {
    if (playing) {
      val i = dom.window.requestAnimationFrame(tickFunc(_))
    }
  }

  def stop():Unit = {
    println("stop!")
    playing = false
    rerender()
  }

  def start():Unit = {
    playing = true
    rerender()
    conditionalRepeat()
  }

  override protected def render = <.div(
    if (playing)
      <.button(^.cls := "btn btn-outline-primary", ^.onClick --> stop(), "Stop")
    else
      <.button(^.cls := "btn btn-outline-primary", ^.onClick --> start(), "Start")
  )
}
