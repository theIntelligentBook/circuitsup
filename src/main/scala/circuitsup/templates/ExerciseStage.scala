package circuitsup.templates

import circuitsup.Common
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, VHtmlNode}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge._
import org.scalajs.dom.{Element, Node}


trait ExerciseStage extends Stage {

  val kind = "exercise"

  def isComplete:Boolean = completion match {
    case Complete(_, _) => true
    case _ => false
  }

}

case class OExerciseStage(left: () => VHtmlNode, right: () => VHtmlNode) extends Stage {
  override def completion: Challenge.Completion = Open

  override def kind: String = "exercise"

  override protected def render: DiffNode[Element, Node] = {
    <.div( Challenge.textAndEx(left())(right()) )
  }
}
