package circuitsup

import com.wbillingsley.veautiful.templates.HistoryRouter
import com.wbillingsley.veautiful.{<, ElementComponent, PathDSL}

sealed trait ExampleRoute
case object IntroRoute extends ExampleRoute
case object CircuitsRoute extends ExampleRoute

object Router extends HistoryRouter[ExampleRoute] {

  var route:ExampleRoute = IntroRoute

  def rerender() = renderElements(render())

  def render() = {
    route match {
      case IntroRoute => Intro.page
      case CircuitsRoute => Circuits.page
    }
  }

  override def path(route: ExampleRoute): String = {
    import PathDSL._

    route match {
      case IntroRoute => (/# / "").stringify
      case CircuitsRoute => (/# / "circuits").stringify
    }
  }

  override def routeFromLocation(): ExampleRoute = PathDSL.hashPathArray() match {
    case Array("") => IntroRoute
    case Array("circuits") => CircuitsRoute
    case x =>
      println(s"path was ${x}")
      IntroRoute
  }

}
