package circuitsup.binary

import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, VHtmlComponent}
import com.wbillingsley.wren.Binary
import org.scalajs.dom
import org.scalajs.dom.{Element, Node}

class Clock(showHex:Boolean = true) extends VHtmlComponent {

  var counter:Byte = 0

  val tick: () => Unit = { ()=>
    counter = (counter + 1).toByte
    rerender()
  }

  var timer:Option[Int] = None

  override protected def render: DiffNode[Element, Node] = <.div(Binary.unsigned8bit(counter, showHex))

  override def afterAttach(): Unit = {
    super.afterAttach()
    timer = Some(dom.window.setInterval(tick, 500))
  }

  override def afterDetach(): Unit = {
    for { t <- timer } {
      timer = None
      dom.window.clearTimeout(t)
    }
    super.afterDetach()
  }

}
