package circuitsup

import circuitsup.analog.Analog
import circuitsup.templates.FrontPage
import com.wbillingsley.veautiful.PathDSL
import com.wbillingsley.veautiful.html.{<, ^}
import com.wbillingsley.veautiful.templates.HistoryRouter

sealed trait Route
case object IntroRoute extends Route
case object CircuitsRoute extends Route

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
      CircuitsRoute -> Analog.topic
    )
  )

  def rerender() = renderElements(render())

  def render() = {
    route match {
      case IntroRoute => frontPage.layout
      case CircuitsRoute => Analog.challenge
    }
  }

  override def path(route: Route): String = {
    import com.wbillingsley.veautiful.PathDSL._

    route match {
      case IntroRoute => (/# / "").stringify
      case CircuitsRoute => (/# / "circuits").stringify
    }
  }

  override def routeFromLocation(): Route = PathDSL.hashPathArray() match {
    case Array("") => IntroRoute
    case Array("circuits") => CircuitsRoute
    case x =>
      println(s"path was ${x}")
      IntroRoute
  }

}
