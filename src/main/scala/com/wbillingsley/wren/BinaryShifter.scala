package com.wbillingsley.wren

import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, VHtmlComponent, VHtmlNode, ^}
import org.scalajs.dom.{Element, Node}

class BinaryShifter(initial:Int, bits:Int = 8, ops:Seq[String]=Seq.empty, signed:Boolean=false, target:Option[Int] = None)(val onUpdate: () => Unit) extends VHtmlComponent {

  var number:Int = initial

  override protected def render: DiffNode[Element, Node] = <.div(
    Binary.showBinary(number, bits, signed=signed),
    <.div(<.div(^.cls := "btn btn-group", ops.map(opsMap(_).render)))
  )

  def bit(n:Int):Boolean = (number & (1 << n)) != 0

  def signBit:Boolean = bit(bits - 1)

  def reset(): Unit = {
    number = initial
    onUpdate()
    rerender()
  }

  sealed trait Op {
    def render:VHtmlNode
    def apply():Unit
  }

  object Reset extends Op {
    def render = <.button(^.cls := "btn btn-secondary", ^.onClick --> apply(), "Reset")
    def apply() = reset()
  }

  object LSL extends Op {
    def render = <.button(^.cls := "btn btn-secondary", ^.onClick --> apply(), "LSL")
    def apply() = {
      number = number << 1
      onUpdate()
      rerender()
    }
  }

  object ASR extends Op {
    def render = <.button(^.cls := "btn btn-secondary", ^.onClick --> apply(), "ASR")
    def apply() = {
      if (signBit) {
        number = (number >> 1) | (1 << (bits - 1))
      } else {
        number = number >> 1
      }
      onUpdate()
      rerender()
    }
  }

  object LSR extends Op {
    def render = <.button(^.cls := "btn btn-secondary", ^.onClick --> apply(), "LSR")
    def apply() = {
      number = number >>> 1
      onUpdate()
      rerender()
    }
  }

  object ROL extends Op {
    def render = <.button(^.cls := "btn btn-secondary", ^.onClick --> apply(), "ROL")
    def apply() = {
      number = if (signBit) (number << 1) | 1 else (number << 1)
      onUpdate()
      rerender()
    }
  }

  object ROR extends Op {
    def render = <.button(^.cls := "btn btn-secondary", ^.onClick --> apply(), "ROR")
    def apply() = {
      number = if (bit(0)) (number >>> 1) | (1 << (bits - 1)) else (number >>> 1)
      onUpdate()
      rerender()
    }
  }

  val opsMap:Map[String, Op] = Map(
    BinaryShifter.Reset -> Reset, BinaryShifter.LSL -> LSL, BinaryShifter.ASR -> ASR, BinaryShifter.LSR -> LSR,
    BinaryShifter.ROL -> ROL, BinaryShifter.ROR -> ROR
  )

}

object BinaryShifter {

  val Reset = "reset"
  val LSL = "lsl"
  val ASL = LSL
  val LSR = "lsr"
  val ASR = "asr"
  val ROL = "rol"
  var ROR = "ror"

}
