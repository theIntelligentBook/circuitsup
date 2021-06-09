package circuitsup

import circuitsup.analog.Analog
import circuitsup.binary.BinaryTopic
import circuitsup.booleanlogic.BooleanTopic
import circuitsup.latches.LatchesTopic
import circuitsup.mosfets.Topic2
import circuitsup.templates.{FrontPage, Topic}
import com.wbillingsley.veautiful.PathDSL
import com.wbillingsley.veautiful.html.{<, ^}
import com.wbillingsley.veautiful.templates.HistoryRouter

sealed trait Route
case object IntroRoute extends Route
case class CircuitsRoute(l:Int, s:Int) extends Route
case class MosfetsRoute(l:Int, s:Int) extends Route
case class BooleanRoute(l:Int, s:Int) extends Route
case class BinaryRoute(l:Int, s:Int) extends Route
case class LatchesRoute(l:Int, s:Int) extends Route

object Router extends HistoryRouter[Route] {

  var route:Route = IntroRoute

  val frontPage = new FrontPage(
    <.div(
      <.h1("Circuits Up!")
    ),
    <.div(
      <.p(^.cls := "lead",
         """
          | These are a set of videos and active learning exercises that try to teach how computers work "from circuits up".
          |""".stripMargin
      ),
      <.p(
        """
          | Of course, modern computers are quite a lot more complex than this, but we hope this gives some intuition for
          | what computers do, for students starting out.
          |""".stripMargin
      )
    ),
    Seq(
      CircuitsRoute(0, 0) -> Analog.topic,
      MosfetsRoute(0, 0) -> Topic2.topic,
      BooleanRoute(0, 0) -> BooleanTopic.topic,
      BinaryRoute(0, 0) -> BinaryTopic.topic,
      LatchesRoute(0, 0) -> LatchesTopic.topic,
    )
  )

  def rerender() = renderElements(render)

  def render = {
    route match {
      case IntroRoute => frontPage.layout
      case CircuitsRoute(l, s) => Analog.challenge.show(l, s)
      case MosfetsRoute(l, s) => Topic2.challenge.show(l, s)
      case BooleanRoute(l, s) => BooleanTopic.challenge.show(l, s)
      case BinaryRoute(l, s) => BinaryTopic.challenge.show(l, s)
      case LatchesRoute(l, s) => LatchesTopic.challenge.show(l, s)
    }
  }

  override def path(route: Route): String = {
    import com.wbillingsley.veautiful.PathDSL.Compose._

    route match {
      case IntroRoute => (/# / "").stringify
      case CircuitsRoute(l, s) => (/# / "circuits" / l.toString / s.toString).stringify
      case MosfetsRoute(l, s) => (/# / "mosfets" / l.toString / s.toString).stringify
      case BooleanRoute(l, s) => (/# / "boolean" / l.toString / s.toString).stringify
      case BinaryRoute(l, s) => (/# / "binary" / l.toString / s.toString).stringify
      case LatchesRoute(l, s) => (/# / "latches" / l.toString / s.toString).stringify
    }
  }


  def parseInt(s:String, or:Int):Int = {
    try {
      s.toInt
    } catch {
      case n:NumberFormatException => or
    }
  }
  override def routeFromLocation(): Route = PathDSL.hashPathArray() match {
    case Array("") => IntroRoute
    case Array("circuits", l, s) => CircuitsRoute(parseInt(l, 0), parseInt(s, 0))
    case Array("mosfets", l, s) => MosfetsRoute(parseInt(l, 0), parseInt(s, 0))
    case Array("boolean", l, s) => BooleanRoute(parseInt(l, 0), parseInt(s, 0))
    case Array("binary", l, s) => BinaryRoute(parseInt(l, 0), parseInt(s, 0))
    case Array("latches", l, s) => LatchesRoute(parseInt(l, 0), parseInt(s, 0))
    case x =>
      println(s"path was ${x}")
      IntroRoute
  }

}
