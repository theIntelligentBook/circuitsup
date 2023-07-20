package circuitsup.booleanlogic

import circuitsup.{BooleanRoute, CircuitsRoute, Common, IntroRoute, Main, MosfetsRoute, Router}
import circuitsup.templates.{Topic, YouTubeStage}
import com.wbillingsley.veautiful.html.{<, VHtmlContent, ^}
import com.wbillingsley.veautiful.doctacular.Challenge
import com.wbillingsley.veautiful.doctacular.Challenge.{Complete, Level}

object BooleanTopic {

  implicit val onCompletionUpdate: () => Unit = () => challenge.rerender()

  implicit val nextButton: () => VHtmlContent = () => {
    challenge.next match {
      case Some((l, s)) => <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(BooleanRoute(l, s)), s"Next")
      case _ => <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(IntroRoute), s"Home")
    }
  }

  val topic = new Topic(
    name = "Boolean Logic",

    image = <.div(),

    content = <.div("Hello!"),

    cssClass = "boolean",

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
    Level(("Boolean Logic"), Seq(
      YouTubeStage("t1hImyElbzk")
    )),
    Level("And and Or", Seq(
      AndGateMosfets, AndGateLogic, OrGateMosfets, OrGateLogic
    )),
    Level("Nand and Nor", Seq(
      NandGateLogic, NandGateMosfets, NorGateLogic, NorGateMosfets
    )),
    Level("XOR and XNOR", Seq(
      XorGateLogic, XnorGateLogic
    )),
    Level("Half and Full Adders", Seq(
      HalfAdderLogic, FullAdderLogic
    )),
  )

  val challenge:Challenge = Common.makeChallenge(BooleanRoute.apply, levels)

}
