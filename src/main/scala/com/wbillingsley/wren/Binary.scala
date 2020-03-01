package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{<, VHtmlNode, ^}

object Binary {

  def unsigned8bit(num:Byte, showHex:Boolean = true):VHtmlNode = {
    <.table(^.cls := "binary-table",
      <.tr(
        for { s <- Seq("128", "64", "32", "16", "8", "4", "2", "1") } yield <.th(^.cls := "number-column", s),
        if (showHex) <.th(^.cls := "hex-string", "Hex") else "",
        <.th(^.cls := "decimal-string", "Decimal")
      ),
      <.tr(
        for { i <- Seq(128, 64, 32, 16, 8, 4, 2, 1) } yield <.td(
          if (num.&(i) != 0) "1" else "0"
        ),
        if (showHex) <.td(^.cls := "hex-string", (0xff & num).formatted("%02X")) else "",
        <.td(^.cls := "decimal-string", (0xff & num).toString)
      )
    )
  }

}
