package circuitsup.binary

import circuitsup.templates.ExerciseStage
import BinaryTopic.nextButton
import BinaryTopic.onCompletionUpdate
import circuitsup.Common
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, VHtmlComponent}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.wren.Binary
import org.scalajs.dom
import org.scalajs.dom.{Element, Node}

object CountingInBinary extends ExerciseStage {

  override def completion: Challenge.Completion = Challenge.Open

  object Clock extends VHtmlComponent {

    var counter:Byte = 0

    val tick: () => Unit = { ()=>
      counter = (counter + 1).toByte
      rerender()
    }

    var timer:Option[Int] = None

    override protected def render: DiffNode[Element, Node] = <.div(Binary.unsigned8bit(counter))

    override def afterAttach(): Unit = {
      super.afterAttach()
      timer = Some(dom.window.setInterval(tick, 500))
    }

    override def afterDetach(): Unit = {
      for { t <- timer } {
        timer = None
        dom.window.clearTimeout(t)
      }
      super.afterDetach()
    }

  }

  override protected def render: DiffNode[Element, Node] = {
    <.div(Challenge.textAndEx(
      <.div(
        Common.marked(
          """
            | # Counting in Binary
            |
            | Still being built...
            |""".stripMargin
        ),
      )
    )(
      Challenge.textColumn(
        Clock
      )
    ))
  }
}
