package circuitsup.mosfets

import circuitsup.{CircuitsRoute, Common, IntroRoute, Main, MosfetsRoute, Router}
import circuitsup.templates.{Topic, YouTubeStage}
import com.wbillingsley.veautiful.html.{<, VHtmlContent, ^}
import com.wbillingsley.veautiful.doctacular.Challenge
import com.wbillingsley.veautiful.doctacular.Challenge.{Complete, Level}

object Topic2 {

  implicit val onCompletionUpdate: () => Unit = () => challenge.rerender()

  implicit val nextButton: () => VHtmlContent = () => {
    challenge.next match {
      case Some((l, s)) => <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(MosfetsRoute(l, s)), s"Next")
      case _ => <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(IntroRoute), s"Home")
    }
  }

  val f = List(1, 2, 3).prependedAll[Int](Array(1, 2, 3))


  val topic = new Topic(
    name = "MOSFETs and Digital Logic",

    image = <.div(),

    content = <.div("Hello!"),

    cssClass = "mosfets",

    completion = () => {
      val allStages = challenge.levels.flatMap(_.stages)

      val done = (100.0 * allStages.count(_.completion match {
        case Complete(_, _) => true
        case _ => false
      }) / allStages.length.toDouble).toInt

      s"${done}%"
    }
  )

  val levels = Seq(
    Level("MOSFETs and Logic", Seq(YouTubeStage("i5TNTXnhCAc"))),
    Level("NMOS", Seq(
      NMos.Page1, NMos.Page2, NMos.Page3, NMos.Page4
    )),
    Level("PMOS", Seq(
      PMos.Page1, PMos.Page2, PMos.Page3, PMos.Page4
    )),
    Level("CMOS", Seq(
      CMos.Page1, CMos.Page2, CMos.Page3, CMos.Page4
    ))
  )

  val c = "MOSFETs and Digi"

  val challenge:Challenge = Common.makeChallenge(MosfetsRoute.apply, levels)

}
