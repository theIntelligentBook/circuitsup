package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{<, VHtmlElement, ^}

object Binary {

  def unsignedNibble(num:Byte, showHex:Boolean = true) = {
    showBinary(num, 4, showHex=showHex, signed=false)
  }


  /**
   * Produces a string representation of an Int, if it was only (bits) long.
   * @param n
   * @param bits
   * @param signed
   * @return
   */
  def decimal(n:Int, bits:Int = 8, signed:Boolean = false):String = {
    if (signed && ((n & (1 << (bits - 1))) != 0))
      (0xff & n).toByte.toString
    else
      (0xff & n).toString
  }


  def showBinary(num:Int, bits:Int, powers:Boolean=false, signed:Boolean=false, showHex:Boolean = true, showDecimal:Boolean=true, divideNibble:Boolean=false) = {

    def numDigits = Math.ceil(bits / 4.0).toInt

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
        if (showDecimal) <.th(^.cls := "decimal-string", "Decimal") else ""
      ),
      <.tr(
        for { i <- (0 until bits).reverse } yield <.td(
          ^.cls := (if (divideNibble && i % 4 == 0) "number-column nibble-end" else "number-column"),
          if (num.&(1 << i) != 0) "1" else "0"
        ),
        if (showHex) <.td(^.cls := "hex-string", (0xff & num).formatted(s"%0${numDigits}X")) else "",
        if (showDecimal) <.td(^.cls := "decimal-string", decimal(num, bits, signed)) else ""
      )
    )
  }


  /** Sums a bit sequence to produce a number */
  def sumBits(bits:Seq[Option[Boolean]]):Option[Int] = bits.zipWithIndex.foldLeft[Option[Int]](Some(0)) {
    case (Some(total), (Some(bit), i)) =>
      if (bit) {
        Some(total + (1 << (bits.length - i - 1)))
      } else Some(total)
    case _ => None
  }

  def unsignedBinOpt(bits:Seq[Option[Boolean]], powers:Boolean=false, showHex:Boolean = true, showDecimal:Boolean = true, divideNibble:Boolean=false) = {

    def numDigits = Math.ceil(bits.length / 4.0).toInt

    val num = sumBits(bits)

    <.table(^.cls := "binary-table",
      <.tr(
        if (powers) {
          for { i <- bits.indices.reverse } yield <.th(
            ^.cls := (if (divideNibble && i % 4 == 0) "number-column nibble-end" else "number-column"),
            "2", <("sup")(i.toString)
          )
        } else {
          for { i <- bits.indices.reverse } yield <.th(
            ^.cls := (if (divideNibble && i % 4 == 0) "number-column nibble-end" else "number-column"),
            (1 << i).toString
          )
        },
        if (showHex) <.th(^.cls := "hex-string", "Hex") else "",
        if (showHex) <.th(^.cls := "decimal-string", "Decimal") else ""
      ),
      <.tr(
        for { (b, i) <- bits.zipWithIndex } yield <.td(
          ^.cls := (if (divideNibble && i % 4 == 0) "number-column nibble-end" else "number-column"),
          b match {
            case Some(true) => "1"
            case Some(false) => "0"
            case _ => "?"
          }
        ),
        if (showHex) <.td(^.cls := "hex-string", num match {
          case Some(n) => (0xff & n).formatted(s"%0${numDigits}X")
          case _ => "?"
        }) else "",
        if (showDecimal) <.td(^.cls := "decimal-string", num match {
          case Some(n) => (0xff & n).toString
          case _ => "?"
        }) else ""
      )
    )
  }


  def unsignedDecimal(num:Int, digits:Int = 8) = {
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
