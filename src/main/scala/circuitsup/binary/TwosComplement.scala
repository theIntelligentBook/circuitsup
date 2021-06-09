package circuitsup.binary

import circuitsup.Common
import circuitsup.binary.BinaryTopic.{nextButton, onCompletionUpdate}
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, ^}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.Complete
import com.wbillingsley.wren.{Binary, BinaryToggle}
import org.scalajs.dom.{Element, Node}

import scala.util.Random

object TwosComplement extends ExerciseStage {

  var completion: Challenge.Completion = Challenge.Open

  val toggle = new BinaryToggle(Random.nextInt.toByte, bits=4, hex=true, showDecimal=false, showTarget=false)(onUpdate)
  def twosC = (0xF & -toggle.number)

  private val targetNums = (1 to 2).map { _ => Random.nextInt(128) }
  val exercises:Seq[BinaryToggle] = targetNums.map { x => new BinaryToggle(-x, signed=true)(onUpdate) }

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
          |# Two's Complement
          |
          |So far, we've considered *positive* numbers. But what about negative numbers? What is `-1` in binary, for instance?
          |
          |In the last exercise, we found that if we have a 4-bit nibble, then `#F` + 1 overflows back to zero
          |""".stripMargin
      ),
      <.p(
        Binary.showBinary(15, 4, showHex=true, showDecimal=false), " + ",
        Binary.showBinary(1, 4, showHex=true, showDecimal=false), " = ",
        Binary.showBinary(0, 4, showHex=true, showDecimal=false)
      ),
      Common.marked(
        """
          |Something similar happens with digital clocks. Midnight is `0:00`, and one minute before midnight is `23:59`
          |(the largest digits it ever shows).
          |
          |In *two's complement*, this is used to decide how to show negative numbers. `-1 + 1 = 0`, so the bit pattern
          |for `-1` is the one that will produce zero when `1` is added to it. For our 4-bit nibbles, that's `#F`, but
          |for a *signed byte* (8-bits) it'd be `#FF`. For a 16-bit signed short, it'd be `#FFFF`.
          |
          |Here's one where you can toggle one of the numbers' bits, and its two's complement will be worked out for you.
          |""".stripMargin),
      <.p(toggle, " + ", Binary.showBinary(twosC, 4, showDecimal=false), " = ", Binary.showBinary(0, 4, showDecimal=false)),
      Common.marked(
        """
          |So if that's the case, is `1000` 8 or -8? And is `0111` 7 or -7?
          |
          |By convention, when using *signed* numbers, the most significant bit (the leftmost bit) is considered the
          |*sign* bit. If it is `1`, the number is negative. So, in a signed 4-bit number, `1000` would be -8, but `0111` would be +7.
          |
          |Typically, we don't have signed 4-bit numbers, but we do have *signed bytes* (8-bits) that can store numbers
          |from -128 (`10000000`) up to 127 (`01111111`).
          |
          |""".stripMargin
      )

    )(Challenge.textColumn(
      <.div(^.cls := "card",
        <.div(^.cls := "card-header", "Exercises"),
        <.div(^.cls := "card-body",
          <.p("Let's show you a positive number and ask you to work out its two's complement negative number. A quick way is to invert all the bits and then add 1."),
          <.ol(
            for { ex <- exercises } yield <.li(
              <.p(Binary.showBinary(-ex.target, 8, showHex = false)),
              <.p(ex)
            )
          )
        )

      ),
      if (isComplete) <.div(
        <.p("As well as signed bytes, there are signed shorts (16 bits), signed ints (usually 32 bits), and signed 64 bit numbers. But they're probably a bit big to put on this page!"),
        nextButton()
      ) else <.p()
    ))
  )

}
