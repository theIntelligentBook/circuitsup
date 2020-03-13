package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{<, VHtmlNode, ^}

object Binary {

  def powersHeader(base:String):Seq[VHtmlNode] = {
    for { i <- Seq("7", "6", "5", "5", "3", "2", "1", "0") } yield <.th(^.cls := "number-column", base, <("sup")(i)),
  }

  def literal2sHeader:Seq[VHtmlNode] = for {
    s <- Seq("128", "64", "32", "16", "8", "4", "2", "1")
  } yield <.th(^.cls := "number-column", s)

  def unsigned8bit(num:Byte, header:Seq[VHtmlNode] = literal2sHeader, showHex:Boolean = true):VHtmlNode = {
    <.table(^.cls := "binary-table",
      <.tr(
        header,
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

  def unsignedDecimal(num:Int):VHtmlNode = {
    var remainder = num

    <.table(^.cls := "binary-table",
      <.tr(
        powersHeader("10"),
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
