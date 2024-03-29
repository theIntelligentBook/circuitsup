package circuitsup.templates

import circuitsup.{Common, Route, Router}
import com.wbillingsley.veautiful.html.{<, VHtmlContent, ^}

case class FrontPage(banner:VHtmlContent, topMessage:VHtmlContent, topics:Seq[(Route, Topic)]) {

  def layout = {
    Common.shell(
      <.div(^.cls := "front-page",
        <.div(^.cls := "course-banner", banner),
        <.div(^.cls := "top-message", topMessage),
        <.div(^.cls := "topic-container",
          for { (r, t) <- topics } yield t.block(Router.path(r))
        )
      )
    )
  }


}
