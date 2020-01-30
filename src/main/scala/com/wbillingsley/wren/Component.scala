package com.wbillingsley.wren

import com.wbillingsley.veautiful.html.{VHtmlComponent, VHtmlDiffNode}

trait Component extends VHtmlComponent {

  def terminals:Seq[Terminal]

  def constraints:Seq[Constraint]

  def render:VHtmlDiffNode

}

object Component {

  type ColouringRule = () => String

}


