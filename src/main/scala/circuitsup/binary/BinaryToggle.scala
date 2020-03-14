package circuitsup.binary

import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, VHtmlComponent, VHtmlNode, ^}
import com.wbillingsley.wren.Binary
import org.scalajs.dom.{Element, Node}

class BinaryToggle(val target:Int, initial:Int = 0, bits:Int = 8, divideNibble:Boolean = false, signed:Boolean = false, hex:Boolean = false, showDecimal:Boolean = true, showTarget:Boolean = true)(onUpdate: () => Unit) extends VHtmlComponent {

  private var num = initial

  def number = num

  def isComplete = (0xff & num) == (0xff & target)

  def numDigits = Math.ceil(bits / 4.0).toInt

  def update(x:Int) = {
    num = (num & 0xff) ^ (x & 0xff)
    onUpdate()
    rerender()
  }

  def decimal(n:Int):String = {
    if (signed && ((n & (1 << (bits - 1))) != 0))
      (0xff & n).toByte.toString
    else
      (0xff & n).toString
  }

  override protected def render: DiffNode[Element, Node] = {
    <.table(^.cls := "binary-table",
      <.tr(
        for { i <- (0 until bits).reverse } yield <.th(
          ^.cls := (if (divideNibble && i % 4 == 0) "number-column nibble-end" else "number-column"),
          (1 << i).toString
        ),
        if (hex) <.th(^.cls := "hex-string", "Hex") else "",
        if (showDecimal) <.th(^.cls := "decimal-string", "Decimal") else "",
        if (showTarget) <.th(^.cls := (if (isComplete) "target-string complete" else "target-string"), if (hex) "Hex target" else "Target") else ""
      ),
      <.tr(
        for { i <- (0 until bits).reverse } yield <.td(^.cls := (if (divideNibble && i % 4 == 0) "number-column nibble-end" else "number-column"),
          <.button(^.cls := "btn btn-outline-secondary", ^.onClick --> update(1 << i), if (((1 << i) & num) != 0) "1" else "0")
        ),
        if (hex) <.td(^.cls := "hex-string", (0xff & num).formatted(s"%0${numDigits}X")) else "",
        if (showDecimal) <.td(^.cls := "decimal-string", decimal(num)) else "",
        if (showTarget) <.td(^.cls := (if (isComplete) "target-string complete" else "target-string"),
          if (hex) (0xff & target).formatted(s"%0${numDigits}X") else decimal(target)
        ) else ""
      )
    )

  }
}
