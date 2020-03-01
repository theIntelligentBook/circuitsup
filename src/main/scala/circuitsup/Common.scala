package circuitsup

import circuitsup.templates.Markup
import com.wbillingsley.veautiful.html.{<, SVG, VHtmlNode, ^}
import com.wbillingsley.veautiful.templates.Challenge

import scala.scalajs.js

/**
  * Common UI components to all the views
  */
object Common {

  val routes:Seq[(Route, String)] = Seq(
    IntroRoute -> "Hello world",
    CircuitsRoute(0, 0) -> "Currents, Voltages, & Resistances",
    MosfetsRoute(0, 0) -> "MOSFETs and Digital Logic"
  )

  def linkToRoute(r:Route, s:String):VHtmlNode = <.a(
    ^.href := Router.path(r),
    ^.cls := (if (Router.route == r) "nav-link active" else "nav-link"),
    s
  )

  def leftMenu:VHtmlNode = <("nav")(^.cls := "d-none d-md-block bg-light sidebar",
    <.div(^.cls := "sidebar-sticky",
      <.ul(^.cls := "nav nav-pills flex-column",
        for { (r, t) <- routes } yield <.li(
          ^.cls := "nav-item",
          linkToRoute(r, t)
        )
      )
    )
  )

  def layout(ch:VHtmlNode) = shell(<.div(^.cls := "move-content-down",
    <.div(^.cls := "row",
      <.div(^.cls := "col-sm-3", leftMenu),
      <.div(^.cls := "col-sm-9", ch)
    )
  ))

  def shell(ch:VHtmlNode) = <.div(
    <("nav")(^.cls := "navbar navbar-dark fixed-top bg-dark flex-md-nowrap p-0 shadow",
      <.div(^.cls := "container",
        <.a(^.cls := "navbar-brand col-sm-3 col-md-2 mr-0", ^.href := "#", "")
      )
    ),

    <.div(^.cls := "container", ch)
  )

  def marked(text: => String) = Markup.marked.MarkupNode(() => text)

  /** Circuits Up! Logo */
  def symbol = {
    <.svg(^.cls := "circuits-up-symbol", ^.attr("viewBox") := "0 6 100 80",
      SVG.path(^.attr("d") := "M 80 16 l 0 -8 l -64 0 l 0 16 l 16 4 l -32 8 l 32 8 l -32 8 l 16 4 l 0 16 l 64 0 l 0 -8"),
      SVG.circle(^.attr("cx") := 80, ^.attr("cy") := 16, ^.attr("r") := 3),
      SVG.circle(^.attr("cx") := 80, ^.attr("cy") := 64, ^.attr("r") := 3),
      SVG.path(^.attr("d") := "M 64 40 l 16 -8 l 16 8 m -16 -8 l 0 16")
    )
  }

  def notS(s:String):String = {
    //s"<span style='text-decoration: overline;'>$s</span>"
    s"${s}&#773;"
  }

  def makeChallenge(routeFunction: (Int, Int) => Route, levels:Seq[Challenge.Level]):Challenge = {

    Challenge(
      levels,
      homePath = (_) => Router.path(IntroRoute),
      levelPath = (_, i) => Router.path(routeFunction(i, 0)),
      stagePath = (_, i, j) => Router.path(routeFunction(i, j)),
      homeIcon = Common.symbol,
      scaleToWindow = Main.scaleChallengesToWindow
    )


  }

}
