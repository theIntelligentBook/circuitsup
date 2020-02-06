package circuitsup.mosfets

import circuitsup.{CircuitsRoute, Common, IntroRoute, MosfetsRoute, Router}
import circuitsup.templates.{Topic, YouTubeStage}
import com.wbillingsley.veautiful.html.{<, VHtmlNode, ^}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Level}

object Topic2 {

  implicit val onCompletionUpdate: () => Unit = () => challenge.rerender()

  implicit val nextButton: () => VHtmlNode = () => {
    challenge.next match {
      case Some((l, s)) => <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(MosfetsRoute(l, s)), s"Next")
      case _ => <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(MosfetsRoute(0, 0)), s"Start")
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
    Level("MOSFETs and Logic", Seq(YouTubeStage("0bcvcJwUE-8"))),
    Level("NMOS", Seq(
      NMos.Page1, NMos.Page2, NMos.Page3, NMos.Page4
    )),
    Level("PMOS", Seq(
      PMos.Page1, PMos.Page2, PMos.Page3, PMos.Page4
    )),
    Level("CMOS", Seq(
      CMos.Page1, CMos.Page2
    ))
  )

  val challenge:Challenge = Challenge(
    levels,
    homePath = (_) => Router.path(IntroRoute),
    levelPath = (_, i) => Router.path(MosfetsRoute(i, 0)),
    stagePath = (_, i, j) => Router.path(MosfetsRoute(i, j)),
    homeIcon = Common.symbol
  )

}
