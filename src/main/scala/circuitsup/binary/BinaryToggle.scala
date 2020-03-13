package circuitsup.binary

import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, VHtmlComponent, VHtmlNode, ^}
import com.wbillingsley.wren.Binary
import org.scalajs.dom.{Element, Node}

class BinaryToggle(target:Int, initial:Int = 0, signed:Boolean = false, showHex:Boolean = false)(onUpdate: () => Unit) extends VHtmlComponent {

  private var num = initial

  def isComplete = num == target

  def update(x:Int) = {
    num = (num & 0xff) ^ (x & 0xff)
    onUpdate()
    rerender()
  }

  override protected def render: DiffNode[Element, Node] = {
    <.table(^.cls := "binary-table",
      <.tr(
        Binary.literal2sHeader,
        if (showHex) <.th(^.cls := "hex-string", "Hex") else "",
        <.th(^.cls := "decimal-string", "Decimal"),
        <.th(^.cls := (if (isComplete) "target-string complete" else "target-string"), "Target")
      ),
      <.tr(
        for { i <- Seq(128, 64, 32, 16, 8, 4, 2, 1) } yield <.td(
          <.button(^.cls := "btn btn-outline-secondary", ^.onClick --> update(i), if ((i & num) != 0) "1" else "0")
        ),
        if (showHex) <.td(^.cls := "hex-string", (0xff & num).formatted("%02X")) else "",
        <.td(^.cls := "decimal-string", (0xff & num).toString),
        <.td(^.cls := (if (isComplete) "target-string complete" else "target-string"), (0xff & target).toString)
      )
    )

  }
}
