package circuitsup.analog

import circuitsup.{CircuitsRoute, Common, IntroRoute, Main, Router}
import circuitsup.templates.{ExerciseStage, Topic, YouTubeStage}
import com.wbillingsley.veautiful.html.{<, SVG, VHtmlNode, ^}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Level, Stage}

object Analog {

  implicit val onCompletionUpdate: () => Unit = () => Analog.challenge.rerender()

  implicit val nextButton: () => VHtmlNode = () => {
    Analog.challenge.next match {
      case Some((l, s)) => <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(CircuitsRoute(l, s)), s"Next")
      case _ => <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(CircuitsRoute(0, 0)), s"Start")
    }
  }

  val f = List(1, 2, 3).prependedAll[Int](Array(1, 2, 3))


  val topic = new Topic(
    name = "Currents, Voltages, and Resistances",

    image = <.div(),

    content = <.div("Hello!"),

    cssClass = "circuits",

    completion = () => {
      val allStages = Analog.challenge.levels.flatMap(_.stages)

      val done = (100.0 * allStages.count(_.completion match {
        case Complete(_, _) => true
        case _ => false
      }) / allStages.length.toDouble).toInt

      s"${done}%"
    }
  )

  val levels = Seq(
    Level("Intro to Circuits", Seq(YouTubeStage("0bcvcJwUE-8"))),
    Level("Kirchhoff's Current Law", Seq(
      KCL.page1, KCL.page2, KCL.page3, KCL.page4, KCL.page5
    )),
    Level("Ohm's Law", Seq(
      Ohms.page1, Ohms.page2, Ohms.page3
    )),
    Level("Kirchhoff's Voltage Law", Seq(
      KVL.page1, KVL.page2, KVL.page3
    )),
    Level("Going Digital", Seq(
      GoingDigital.page1, GoingDigital.page2
    )),
  )

  val challenge:Challenge = Challenge(
    levels,
    homePath = (_) => Router.path(IntroRoute),
    levelPath = (_, i) => Router.path(CircuitsRoute(i, 0)),
    stagePath = (_, i, j) => Router.path(CircuitsRoute(i, j)),
    homeIcon = Common.symbol,
    scaleToWindow = Main.scaleChallengesToWindow
  )

}
