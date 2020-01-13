package circuitsup.templates

import circuitsup.Common
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, VHtmlNode}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge._
import org.scalajs.dom.{Element, Node}

case class ExerciseStage(left: () => VHtmlNode, right: () => VHtmlNode) extends Stage {
  override def completion: Challenge.Completion = Open

  override def kind: String = "exercise"

  override protected def render: DiffNode[Element, Node] = {
    <.div( Challenge.textAndEx(left())(right()) )
  }
}
