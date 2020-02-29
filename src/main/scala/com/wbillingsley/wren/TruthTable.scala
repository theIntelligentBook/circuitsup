package com.wbillingsley.wren

case class TruthTable(inputNames:Seq[String], values:Map[Seq[Boolean], Boolean], current:Seq[Boolean]) {

  def headerNames:String = (for { n <- inputNames } yield s"<th>$n</th>").mkString("")

  def row(rowBinary:Int):String = {
    val lookup = (for { x <- inputNames.indices } yield ((1 << x) & rowBinary) != 0)
    val active = current == lookup
    val output = values.get(lookup) match {
      case Some(true) => "1"
      case Some(false) => "0"
      case None => "?"
    }

    val inputString = (for { x <- lookup } yield {
      s"<td>${ if (x) "1" else "0" }</td>"
    }).mkString

    s"<tr class='${ if (active) "active" else ""}'>$inputString<td>$output</td></tr>"
  }

  def rows = {
    val range = (0 until 1 << inputNames.length).reverse

    (for { r <- range } yield {
      row(r)
    }).mkString
  }

  def htmlString:String =
    s"""
       |<table class="truth-table">
       |<thead><tr>$headerNames<th>Output</th></tr></thead>
       |<tbody>
       |  $rows
       |</tbody>
       |</table>
       |""".stripMargin

}
