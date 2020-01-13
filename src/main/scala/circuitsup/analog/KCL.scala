package circuitsup.analog

import com.wbillingsley.wren.{Circuit, CurrentSource, Orientation, Resistor, ValueLabel, ValueSlider, VoltageSource, Wire}

object KCL {

  val playBackExample = {
    val cs = new CurrentSource((400, 100), initial = Some(0.2))
    val vs = new VoltageSource((100, 150), orientation = Orientation.North, initial = Some(5))
    val r1 = new Resistor((300,100), orientation = Orientation.South)
    val r2 = new Resistor((300,200), orientation = Orientation.South)

    val circuit:Circuit = Circuit(
      Seq(
        r1, r2, vs,
        new Wire(vs.t2, r1.t1, 100 -> 50, 300 -> 50),
        new Wire(r1.t2, r2.t1),
        new Wire(r2.t2, vs.t1, 300 -> 300, 100 -> 300),
        new ValueLabel(vs.voltage, (30, 150)),
        new ValueSlider(vs.voltage, (0, 160), values = Seq("1", "2", "3", "4", "5"), onUpdate = onUpdate),
        new ValueLabel(r1.resistance, (330, 100)),
        new ValueSlider(r1.resistance, (330, 110), values = Seq("1", "2", "3", "4", "5"), onUpdate = onUpdate),
        new ValueLabel(r2.resistance, (330, 200)),
        new ValueSlider(r2.resistance, (330, 210), values = Seq("1", "2", "3", "4", "5"), onUpdate = onUpdate),
        new ValueLabel(r1.voltage, (250, 100)),
        new ValueLabel(r2.voltage, (250, 200)),
        new ValueLabel(r1.t1.current, (100, 10))
      ),
      640, 480
    )




  }


}
