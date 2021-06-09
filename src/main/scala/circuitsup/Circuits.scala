package circuitsup

import com.wbillingsley.veautiful.templates.VSlides
import com.wbillingsley.veautiful.html.{<, ^}

object Circuits {

  def page = {
      VSlides(width=1320, height=800, Seq(
        <.div(
          <("iframe")(
            ^.attr("width") :="1280", ^.attr("height") := "720", ^.src := "https://www.youtube.com/embed/0bcvcJwUE-8",
            ^.attr("frameborder") := "0", ^.attr("allow") :="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture",
            ^.attr("allowfullscreen") := "allowfullscreen")
        ),
        <.div(
          <.h2("Slide 2"),
          <.p("Constraint propagation exercise 1 goes here...")
        )
      ))
  }


}
