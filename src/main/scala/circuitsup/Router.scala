package circuitsup

import circuitsup.analog.Analog
import circuitsup.mosfets.Topic2
import circuitsup.templates.FrontPage
import com.wbillingsley.veautiful.PathDSL
import com.wbillingsley.veautiful.html.{<, ^}
import com.wbillingsley.veautiful.templates.HistoryRouter

sealed trait Route
case object IntroRoute extends Route
case class CircuitsRoute(l:Int, s:Int) extends Route
case class MosfetsRoute(l:Int, s:Int) extends Route

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
      MosfetsRoute(0, 0) -> Topic2.topic
    )
  )

  def rerender() = renderElements(render())

  def render() = {
    route match {
      case IntroRoute => frontPage.layout
      case CircuitsRoute(l, s) => Analog.challenge.show(l, s)
      case MosfetsRoute(l, s) => Topic2.challenge.show(l, s)
    }
  }

  override def path(route: Route): String = {
    import com.wbillingsley.veautiful.PathDSL._

    route match {
      case IntroRoute => (/# / "").stringify
      case CircuitsRoute(l, s) => (/# / "circuits" / l.toString / s.toString).stringify
      case MosfetsRoute(l, s) => (/# / "mosfets" / l.toString / s.toString).stringify
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
    case x =>
      println(s"path was ${x}")
      IntroRoute
  }

}
