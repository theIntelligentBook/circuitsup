package circuitsup.binary

import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, VHtmlComponent, VHtmlNode, ^}
import com.wbillingsley.wren.Binary
import org.scalajs.dom.{Element, Node}

class BinaryToggle(target:Int, initial:Int = 0, bits:Int = 8, divideNibble:Boolean = false, signed:Boolean = false, hex:Boolean = false)(onUpdate: () => Unit) extends VHtmlComponent {

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
        for { i <- (0 until bits).reverse } yield <.th(
          ^.cls := (if (divideNibble && i % 4 == 0) "number-column nibble-end" else "number-column"),
          (1 << i).toString
        ),
        if (hex) <.th(^.cls := "hex-string", "Hex") else "",
        <.th(^.cls := "decimal-string", "Decimal"),
        <.th(^.cls := (if (isComplete) "target-string complete" else "target-string"), if (hex) "Hex target" else "Target")
      ),
      <.tr(
        for { i <- (0 until bits).reverse } yield <.td(^.cls := (if (divideNibble && i % 4 == 0) "number-column nibble-end" else "number-column"),
          <.button(^.cls := "btn btn-outline-secondary", ^.onClick --> update(1 << i), if (((1 << i) & num) != 0) "1" else "0")
        ),
        if (hex) <.td(^.cls := "hex-string", (0xff & num).formatted("%02X")) else "",
        <.td(^.cls := "decimal-string", (0xff & num).toString),
        <.td(^.cls := (if (isComplete) "target-string complete" else "target-string"),
          if (hex) (0xff & target).formatted("%02X") else (0xff & target).toString
        )
      )
    )

  }
}
