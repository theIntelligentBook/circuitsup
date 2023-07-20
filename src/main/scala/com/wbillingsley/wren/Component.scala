package com.wbillingsley.wren

import com.wbillingsley.veautiful.svg.{DSvgComponent, DSvgContent}

trait Component extends DSvgComponent {

  def terminals:Seq[Terminal]

  def constraints:Seq[Constraint]

  def render:DSvgContent

}

object Component {

  type ColouringRule = () => String

}


