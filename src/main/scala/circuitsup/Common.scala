package circuitsup

import com.wbillingsley.veautiful.{<, VNode, ^}

/**
  * Common UI components to all the views
  */
object Common {

  val routes:Seq[(ExampleRoute, String)] = Seq(
    IntroRoute -> "Hello world",
    CircuitsRoute -> "Currents, Voltages, & Resistances"
  )

  def linkToRoute(r:ExampleRoute, s:String):VNode = <.a(
    ^.href := Router.path(r),
    ^.cls := (if (Router.route == r) "nav-link active" else "nav-link"),
    s
  )

  def leftMenu:VNode = <("nav")(^.cls := "d-none d-md-block bg-light sidebar",
    <.div(^.cls := "sidebar-sticky",
      <.ul(^.cls := "nav nav-pills flex-column",
        for { (r, t) <- routes } yield <.li(
          ^.cls := "nav-item",
          linkToRoute(r, t)
        )
      )
    )
  )

  def layout(ch:VNode) = shell(<.div(^.cls := "move-content-down",
    <.div(^.cls := "row",
      <.div(^.cls := "col-sm-3", leftMenu),
      <.div(^.cls := "col-sm-9", ch)
    )
  ))

  def shell(ch:VNode) = <.div(
    <("nav")(^.cls := "navbar navbar-dark fixed-top bg-dark flex-md-nowrap p-0 shadow",
      <.div(^.cls := "container",
        <.a(^.cls := "navbar-brand col-sm-3 col-md-2 mr-0", ^.href := "#", "Circuits Up!")
      )
    ),

    <.div(^.cls := "container", ch)
  )


}
