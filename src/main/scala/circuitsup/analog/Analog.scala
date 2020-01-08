package circuitsup.analog

import circuitsup.{CircuitsRoute, IntroRoute, Router}
import circuitsup.templates.{Topic, YouTubeStage}
import com.wbillingsley.veautiful.html.{<, SVG, ^}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Level, Stage}

object Analog {

  def symbol = {
    <.svg(^.cls := "circuits-up-symbol", ^.attr("viewBox") := "0 6 100 80",
      SVG.path(^.attr("d") := "M 80 16 l 0 -8 l -64 0 l 0 16 l 16 4 l -32 8 l 32 8 l -32 8 l 16 4 l 0 16 l 64 0 l 0 -8"),
      SVG.circle(^.attr("cx") := 80, ^.attr("cy") := 16, ^.attr("r") := 3),
      SVG.circle(^.attr("cx") := 80, ^.attr("cy") := 64, ^.attr("r") := 3),
      SVG.path(^.attr("d") := "M 64 40 l 16 -8 l 16 8 m -16 -8 l 0 16")
    )
  }


  val topic = new Topic(
    name = "Currents, Voltages, and Resistances",

    image = <.p("VIR"),

    content = <.div("Hello!"),

    cssClass = "circuits"
  )

  val challenge = new Challenge(
    Seq(
      Level("Video", Seq(YouTubeStage("0bcvcJwUE-8")))
    ),
    header = <.div(
      <.a(^.cls := "home-link", ^.href := Router.path(IntroRoute), symbol),
      <.span(^.cls := "challenge-name", topic.name)
    )
  )

}
