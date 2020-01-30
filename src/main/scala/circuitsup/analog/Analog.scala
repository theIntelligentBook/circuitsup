package circuitsup.analog

import circuitsup.{CircuitsRoute, IntroRoute, Router}
import circuitsup.templates.{ExerciseStage, Topic, YouTubeStage}
import com.wbillingsley.veautiful.html.{<, SVG, VHtmlNode, ^}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Level, Stage}

object Analog {

  implicit val onCompletionUpdate: () => Unit = () => Analog.challenge.rerender()

  implicit val nextButton: () => VHtmlNode = () => {
    Analog.challenge.next match {
      case Some((l, s)) => <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(CircuitsRoute(l, s)), s"Next")
      case _ => <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(CircuitsRoute(0, 0)), s"Start")
    }
  }

  def symbol = {
    <.svg(^.cls := "circuits-up-symbol", ^.attr("viewBox") := "0 6 100 80",
      SVG.path(^.attr("d") := "M 80 16 l 0 -8 l -64 0 l 0 16 l 16 4 l -32 8 l 32 8 l -32 8 l 16 4 l 0 16 l 64 0 l 0 -8"),
      SVG.circle(^.attr("cx") := 80, ^.attr("cy") := 16, ^.attr("r") := 3),
      SVG.circle(^.attr("cx") := 80, ^.attr("cy") := 64, ^.attr("r") := 3),
      SVG.path(^.attr("d") := "M 64 40 l 16 -8 l 16 8 m -16 -8 l 0 16")
    )
  }

  val f = List(1, 2, 3).prependedAll[Int](Array(1, 2, 3))


  val topic = new Topic(
    name = "Currents, Voltages, and Resistances",

    image = <.p("VIR"),

    content = <.div("Hello!"),

    cssClass = "circuits"
  )

  val levels = Seq(
    Level("Intro to Circuits", Seq(YouTubeStage("0bcvcJwUE-8"))),
    Level("Kirchoff's Current Law", Seq(
      KCL.page1, KCL.page2, KCL.page3, KCL.page4, KCL.page5
    )),
    Level("Ohm's Law", Seq(
      Ohms.page1, Ohms.page3
    )),
    Level("Kirchoff's Voltage Law", Seq.empty),
    Level("Voltage Dividers", Seq.empty),
  )

  val challenge:Challenge = Challenge(
    levels,
    homePath = (_) => Router.path(IntroRoute),
    levelPath = (_, i) => Router.path(CircuitsRoute(i, 0)),
    stagePath = (_, i, j) => Router.path(CircuitsRoute(i, j)),
    homeIcon = symbol
  )

}
