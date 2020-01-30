package circuitsup.templates

import circuitsup.Common
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, VHtmlNode}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge._
import org.scalajs.dom.{Element, Node}


abstract class ExerciseStage(implicit val onCompletionUpdate: () => Unit, implicit val nextButton: () => VHtmlNode) extends Stage {

  val kind = "exercise"

  def isComplete:Boolean = completion match {
    case Complete(_, _) => true
    case _ => false
  }

  def ocu() = {
    rerender()
  }

}