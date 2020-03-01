package circuitsup.binary

import circuitsup.{BooleanRoute, CircuitsRoute, Common, IntroRoute, Main, MosfetsRoute, Router}
import circuitsup.templates.{Topic, YouTubeStage}
import com.wbillingsley.veautiful.html.{<, VHtmlNode, ^}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Level}

object BinaryTopic {

  implicit val onCompletionUpdate: () => Unit = () => challenge.rerender()

  implicit val nextButton: () => VHtmlNode = () => {
    challenge.next match {
      case Some((l, s)) => <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(BooleanRoute(l, s)), s"Next")
      case _ => <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(IntroRoute), s"Home")
    }
  }

  val topic = new Topic(
    name = "Binary and Numbers",

    image = <.div(),

    content = <.div("Hello!"),

    cssClass = "binary",

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
    Level("Binary Numbers", Seq(
      CountingInBinary
    )),
  )

  val challenge:Challenge = Common.makeChallenge(BooleanRoute.apply, levels)

}
