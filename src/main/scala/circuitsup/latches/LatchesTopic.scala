package circuitsup.latches

import circuitsup.templates.Topic
import circuitsup.{BinaryRoute, Common, IntroRoute, LatchesRoute, Router}
import com.wbillingsley.veautiful.html.{<, VHtmlNode, ^}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.{Complete, Level}

object LatchesTopic {

  implicit val onCompletionUpdate: () => Unit = () => challenge.rerender()

  implicit val nextButton: () => VHtmlNode = () => {
    challenge.next match {
      case Some((l, s)) => <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(LatchesRoute(l, s)), s"Next")
      case _ => <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(IntroRoute), s"Home")
    }
  }

  val topic = new Topic(
    name = "Latches and Flip-flops",

    image = <.div(),

    content = <.div("Hello!"),

    cssClass = "latches",

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
    Level("Dynamic discipline", Seq(
      RippleCarryTiming,
    )),
    Level("Latches and Flip Flops", Seq(
      SRLatchLogic, GatedSRLatch, DLatch, DFlipFlop
    )),
    Level("The Clock", Seq(
      FlipFlopsAndClocks, RippleCarryClocked
    )),
  )

  val challenge:Challenge = Common.makeChallenge(LatchesRoute.apply, levels)

}
