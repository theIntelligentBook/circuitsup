package circuitsup.binary

import circuitsup.{BinaryRoute, BooleanRoute, CircuitsRoute, Common, IntroRoute, Main, MosfetsRoute, Router}
import circuitsup.templates.{Topic, YouTubeStage}
import com.wbillingsley.veautiful.html.{<, VHtmlContent, ^}
import com.wbillingsley.veautiful.doctacular.Challenge
import com.wbillingsley.veautiful.doctacular.Challenge.{Complete, Level}

object BinaryTopic {

  implicit val onCompletionUpdate: () => Unit = () => challenge.rerender()

  implicit val nextButton: () => VHtmlContent = () => {
    challenge.next match {
      case Some((l, s)) => <.a(^.cls := "btn btn-outline-secondary pulse-link", ^.href := Router.path(BinaryRoute(l, s)), s"Next")
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
      CountingInBinary, CountingInHexadecimal,
    )),
    Level("Adders", Seq(
      HalfAdderBox, FullAdderBox, RippleCarryAdder, TwosComplement
    )),
    Level("Some other operations", Seq(
      BitwiseOps, BitShifts, BitRotations
    ))
  )

  val challenge:Challenge = Common.makeChallenge(BinaryRoute.apply, levels)

}
