package circuitsup.binary

import circuitsup.templates.ExerciseStage
import BinaryTopic.nextButton
import BinaryTopic.onCompletionUpdate
import circuitsup.Common
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, VHtmlComponent, ^}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.Complete
import com.wbillingsley.wren.{Binary, BinaryToggle}
import org.scalajs.dom
import org.scalajs.dom.{Element, Node}

import scala.util.Random

object CountingInBinary extends ExerciseStage {

  var completion: Challenge.Completion = Challenge.Open

  val clock = new Clock(x => Binary.showBinary(x.toByte, 8, showHex = false))

  val toggle = new BinaryToggle(Random.nextInt.toByte)(onUpdate)

  val exercises:Seq[BinaryToggle] = (1 to 5).map { _ => new BinaryToggle(Random.nextInt(256))(onUpdate) }

  private def checkCompletion = exercises.forall(_.isComplete)

  def onUpdate():Unit = {
    if (checkCompletion) {
      completion = Complete(Some(1), None)
      onCompletionUpdate()
    } else {
      rerender()
    }
  }

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
          |
          |When we count in decimals, we have the digits `0` to `9`. Go past `9` and we run out of symbols and have to
          |move to the next column: `10`. So, children are taught the columns of decimal.
          |The 1s, the 10s, the 100s, the 1000s -- each power of 10. For example, 127 would break down as:
          |""".stripMargin),
      <.p(Binary.unsignedDecimal(127)),
      Common.marked("""
          |1 × 10<sup>2</sup> + 2 × 10<sup>1</sup> + 7 × 10<sup>0</sup> = 100 + 20 + 7 = 127
          |
          |In digital logic, however, we have two fundamental states: high and low, on and off, which we've been showing
          |as `0` or `1`. We only have two numerals to work with, to we have to work in base 2 (binary). So, our columns
          |aren't the powers of 10 but the powers of 2. In *base `2`* (binary), 127 looks like:
          |""".stripMargin),
      <.p(Binary.showBinary(127, 8, powers=true, showHex = false)),
      <.p("Or in other words"),
      <.p(Binary.showBinary(127, 8, showHex = false)),

    )(Challenge.textColumn(
      <.div(^.cls := "card",
        <.div(^.cls := "card-header", "Exercises"),
        <.div(^.cls := "card-body",
          <.p("Toggle the binary bits to match the target number"),
          <.ol(
            for { ex <- exercises } yield <.li(<.p(ex))
          )
        )

      ),
      if (isComplete) <.div(
        <.p("As we can see, converting from binary to decimal or back isn't always intuitive. Let's have a look at an alternative."),
        nextButton()
      ) else <.p()
    ))
  )

}
