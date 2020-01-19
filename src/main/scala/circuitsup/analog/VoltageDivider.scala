package circuitsup.analog

import com.wbillingsley.wren.{Component, CurrentSource, Orientation, Resistor, VoltageSource, Wire}

class VoltageDivider {

  val cs = new CurrentSource((400, 100), initial = Some(0.2))
  val vs = new VoltageSource((100, 150), orientation = Orientation.North, initial = Some(5))
  val r1 = new Resistor((300,100), orientation = Orientation.South)
  val r2 = new Resistor((300,200), orientation = Orientation.South)

  val wires = Seq(
    new Wire(vs.t2, r1.t1, 100 -> 50, 300 -> 50),
    new Wire(r1.t2, r2.t1),
    new Wire(r2.t2, vs.t1, 300 -> 300, 100 -> 300)
  )

  def components:Seq[Component] = Seq(vs, vs, r1, r2) ++ wires

}
