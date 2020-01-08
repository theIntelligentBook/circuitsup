package circuitsup.analog

import circuitsup.{CircuitsRoute, Router}
import circuitsup.templates.Topic
import com.wbillingsley.veautiful.html.<

object Analog {

  val topic = new Topic(
    name = "Currents, Voltages, and Resistances",

    image = <.p("VIR"),

    content = <.div("Hello!"),

    cssClass = "circuits"
  )

}
