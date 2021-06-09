package circuitsup.analog

import com.wbillingsley.wren.{Component, CurrentSource, Orientation, Resistor, Terminal, VoltageSource, Wire}

class OneWireFragment(pos:(Int, Int), height:Int) {

  val t1 = new Terminal(pos)
  val t2 = new Terminal(pos._1 -> (pos._2 + height))
  val wire = new Wire(t1, t2)

  def components:Seq[Component] = Seq(t1, t2, wire)

}
