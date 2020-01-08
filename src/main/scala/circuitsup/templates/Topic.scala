package circuitsup.templates

import com.wbillingsley.veautiful.html.{<, VHtmlNode, ^}

case class Topic(name:String, image:VHtmlNode, content:VHtmlNode, cssClass:String = "") {

  def block(path:String):VHtmlNode = <.div(^.cls := s"topic-block $cssClass",
    <.a(^.href := path,
      <.div(^.cls := "topic-image", image),
      <.div(^.cls := "topic-label", <("label")(name))
    )
  )

}