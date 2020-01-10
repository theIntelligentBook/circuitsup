package circuitsup.analog

import circuitsup.{CircuitsRoute, IntroRoute, Router}
import circuitsup.templates.{Topic, YouTubeStage}
import com.wbillingsley.veautiful.html.{<, SVG, VHtmlNode, ^}
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

  val f = List(1, 2, 3).prependedAll[Int](Array(1, 2, 3))


  val topic = new Topic(
    name = "Currents, Voltages, and Resistances",

    image = <.p("VIR"),

    content = <.div("Hello!"),

    cssClass = "circuits"
  )

  val levels = Seq(
    Level("Intro to Circuits", Seq(YouTubeStage("0bcvcJwUE-8"))),
    Level("Kirchoff's Current Law", Seq.empty),
    Level("Ohm's Law", Seq.empty),
    Level("Kirchoff's Voltage Law", Seq.empty),
    Level("Voltage Dividers", Seq.empty),
  )

  val challenge = new Challenge(
    levels,
    <.div(
      <.a(^.cls := "home-link", ^.href := Router.path(IntroRoute), symbol),
      <.span(^.cls := "challenge-name", topic.name)
    ),
    <.div(),
    progessBlock()
  )


  def progressTile(level:Level, i:Int):VHtmlNode = {
    <.div(^.cls := "progress-level",
      <.div(^.cls := "level-name", level.name),
      <.div(^.cls := "stage-links",
        for {
          (s, j) <- level.stages.zipWithIndex
        } yield {
          <.a(^.cls := "stage-link",
            <("i")(^.cls := "material-icons",
              s.kind match {
                case "video" => "videocam"
                case _ => "lens"
              }
            )
          )
        }
      )
    )
  }

  def progessBlock():VHtmlNode = {
    <.div(^.cls := "progress-block",
      for {
        (l, i) <- levels.zipWithIndex
      } yield progressTile(l, i)
    )
  }

}
