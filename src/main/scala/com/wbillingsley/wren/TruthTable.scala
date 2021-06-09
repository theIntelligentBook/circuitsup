package com.wbillingsley.wren

case class TruthTable(inputNames:Seq[String], outputNames:Seq[String], values:Map[Seq[Boolean], Seq[Boolean]], current:Seq[Boolean]) {

  def headerNames(s:Seq[String]):String = (for { n <- s } yield s"<th>$n</th>").mkString("")

  def row(rowBinary:Int):String = {
    val lookup = (for { x <- inputNames.indices } yield ((1 << x) & rowBinary) != 0)
    val active = current == lookup
    val output = values.get(lookup) match {
      case Some(outputs) =>
        (for { x <- outputs } yield {
          s"<td>${ if (x) "1" else "0" }</td>"
        }).mkString
      case None =>
        (for { _ <- outputNames } yield {
          "<td>?</td>"
        }).mkString
    }

    val inputString = (for { x <- lookup } yield {
      s"<td>${ if (x) "1" else "0" }</td>"
    }).mkString

    s"<tr class='${ if (active) "active" else ""}'>$inputString$output</tr>"
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
       |<thead><tr>${headerNames(inputNames)}${headerNames(outputNames)}</tr></thead>
       |<tbody>
       |  $rows
       |</tbody>
       |</table>
       |""".stripMargin

}
