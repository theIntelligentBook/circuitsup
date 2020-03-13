package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{<, VHtmlNode, ^}

object Binary {

  def unsignedNibble(num:Byte, showHex:Boolean = true):VHtmlNode = {
    unsignedBinary(num, 4, showHex)
  }

  def unsignedBinary(num:Int, bits:Int, powers:Boolean=false, showHex:Boolean = true, divideNibble:Boolean=false):VHtmlNode = {
    <.table(^.cls := "binary-table",
      <.tr(
        if (powers) {
          for { i <- (0 until bits).reverse } yield <.th(
            ^.cls := (if (divideNibble && i % 4 == 0) "number-column nibble-end" else "number-column"),
            "2", <("sup")(i.toString)
          )
        } else {
          for { i <- (0 until bits).reverse } yield <.th(
            ^.cls := (if (divideNibble && i % 4 == 0) "number-column nibble-end" else "number-column"),
            (1 << i).toString
          )
        },
        if (showHex) <.th(^.cls := "hex-string", "Hex") else "",
        <.th(^.cls := "decimal-string", "Decimal")
      ),
      <.tr(
        for { i <- (0 until bits).reverse } yield <.td(
          ^.cls := (if (divideNibble && i % 4 == 0) "number-column nibble-end" else "number-column"),
          if (num.&(1 << i) != 0) "1" else "0"
        ),
        if (showHex) <.td(^.cls := "hex-string", (0xff & num).formatted(s"%0${bits / 4}X")) else "",
        <.td(^.cls := "decimal-string", (0xff & num).toString)
      )
    )
  }

  def unsignedDecimal(num:Int, digits:Int = 8):VHtmlNode = {
    var remainder = num

    <.table(^.cls := "binary-table",
      <.tr(
        for { i <- (0 until digits).reverse } yield <.th(
          ^.cls := "number-column",
          "10", <("sup")(i.toString)
        ),
        <.th(^.cls := "decimal-string", "Decimal")
      ),
      <.tr(
        for { i <- Seq(10000000, 1000000, 100000, 10000, 1000, 100, 10, 1) } yield {
          val d:Int = remainder / i
          remainder -= (d * i)
          <.td(d.toString)
        },
        <.td(^.cls := "decimal-string", num.toString)
      )
    )
  }


}
