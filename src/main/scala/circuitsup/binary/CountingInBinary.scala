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

  val clock = new Clock(false)

  override protected def render: DiffNode[Element, Node] = <.div(
    Challenge.textAndEx(
      Common.marked(
        """
          |# Counting in Binary
          |
          |Sometimes, counting is easier to understand just by watching it. So, let's show a
          |counter incrementing every half-second, in 8-bit binary.
          |""".stripMargin
      ),
      clock,
      Common.marked(
        """
          |When we count in decimals, we have the digits `0` to `9`. Go past `9` and we run out of symbols and have to
          |move to the next column: `10`.
          |
          |So, children are taught the columns of decimal. The 1s, the 10s, the 100s, the 1000s -- each power of 10.
          |
          |""".stripMargin
      )

    )(Challenge.textColumn(<.div()))
  )

}
