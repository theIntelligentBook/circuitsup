package circuitsup.binary

import circuitsup.Common
import circuitsup.binary.BinaryTopic.{nextButton, onCompletionUpdate}
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, ^}
import com.wbillingsley.veautiful.doctacular.Challenge
import com.wbillingsley.veautiful.doctacular.Challenge.Complete
import com.wbillingsley.wren.{Binary, BinaryToggle}
import org.scalajs.dom.{Element, Node}

import scala.util.Random

object CountingInHexadecimal extends ExerciseStage {

  var completion: Challenge.Completion = Challenge.Open

  val clock = new Clock(x => Binary.unsignedNibble(x.toByte), bits = 4)

  val clock2 = new Clock(x => Binary.showBinary(x.toByte, 8, divideNibble = true), bits = 8)

  val exercises:Seq[BinaryToggle] = (1 to 5).map { _ => new BinaryToggle(Random.nextInt(256), divideNibble= true, hex = true)(onUpdate) }

  private def checkCompletion = exercises.forall(_.isComplete)

  def onUpdate():Unit = {
    if (checkCompletion) {
      completion = Complete(Some(1), None)
      onCompletionUpdate()
    } else {
      rerender()
    }
  }


  override protected def render = <.div(
    Challenge.textAndEx(
      Common.marked(
        """
          |# Counting in Hexadecimal
          |
          |Decimal numbers, e.g. `127` are short and fairly easy to think about. Binary numbers, e.g. `01111111` can
          |just be a little long to be memorable. And converting between the two is a hard mental exercise. So, there
          |is another format that many computer scientists will use: *hexadecimal* or base `16`. We use it so much that
          |people often abbreviate it to just "hex".
          |
          |The numbers `0` to `15` can be represented in a four bit "nibble". `15` is `1111`. Because the decimals run out
          |at 9, to show this in *hexadecimal*, we then use the letters `A` to `F` for the numbers `10` to `15`, as in
          |the animation below.
          |""".stripMargin
      ),
      clock,
      Common.marked(
        """
          |Now, every four binary bits can correspond to a hexadecimal numeral. So, if we want to show the value of an
          |8-bit number (or *byte*), our hexadecimal number will be two numerals long:
          |""".stripMargin),
      clock2,
      Common.marked("""
          |To show that a number is written in hexadecimal, rather than decimals, programmers will prepend `#` or `0x`.
          |For example, `#FF` or `0xFF` is the number 255.
          |""".stripMargin),
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
        <.p("After a while, you'll get to recognise some bit patterns (especially `#F` being `1111`)."),
        nextButton()
      ) else <.p()
    ))
  )

}
