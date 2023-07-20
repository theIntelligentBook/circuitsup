package circuitsup.templates

import circuitsup.Common
import com.wbillingsley.veautiful.{DiffNode, Update}
import com.wbillingsley.veautiful.html.{<, VHtmlContent}
import com.wbillingsley.veautiful.doctacular.Challenge
import com.wbillingsley.veautiful.doctacular.Challenge._
import org.scalajs.dom.{Element, Node}


abstract class ExerciseStage(using val onCompletionUpdate: () => Unit, val nextButton: () => VHtmlContent) extends Stage {

  val kind = "exercise"

  def isComplete:Boolean = completion match {
    case Complete(_, _) => true
    case _ => false
  }

  def ocu() = {
    rerender()
  }

}