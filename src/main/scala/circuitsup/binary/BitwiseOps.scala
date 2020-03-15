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

object BitwiseOps extends ExerciseStage {

  var completion: Challenge.Completion = Challenge.Open

  val toggle1 = new BinaryToggle(Random.nextInt.toByte, bits=8, hex=true, showDecimal=false, showTarget=false)(onUpdate)
  val toggle2 = new BinaryToggle(Random.nextInt.toByte, bits=8, hex=true, showDecimal=false, showTarget=false)(onUpdate)

  private def checkCompletion = (toggle1.number == 0xf0) && (toggle2.number == 0xf0)

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
          |# Bitwise Operations
          |
          |If we have two bytes, perhaps we might want to apply a logical operator to the bits of each byte.
          |
          |A bitwise AND (which most programming languages use an `&` for) applies an AND to the corresponding bits in
          |each byte. It's sometimes used if we want to "mask" certain bits. Anything AND zero is zero, so if we
          |put zeroes in our mask, the result of the AND can only contain 1s in the bits we are interested in.
          |""".stripMargin
      ),
      <.p(
        Binary.showBinary(13, 4, showHex=true, showDecimal=false), " & ",
        Binary.showBinary(12, 4, showHex=true, showDecimal=false), " = ",
        Binary.showBinary(4, 4, showHex=true, showDecimal=false)
      ),
      Common.marked(
        """
          |A bitwise OR (which most programming languages use an `|` for) applies an OR to the corresponding bits in
          |each byte. It's sometimes useful to apply an OR mask if we want to ensure some bits in the output are 1.
          |""".stripMargin),
      <.p(
        Binary.showBinary(5, 4, showHex=true, showDecimal=false), " | ",
        Binary.showBinary(12, 4, showHex=true, showDecimal=false), " = ",
        Binary.showBinary(13, 4, showHex=true, showDecimal=false)
      ),
      if (isComplete) <.div(
        <.p("These are most useful if we're treating a byte as a collection of *flags* (individual bits each indicating a true/false) rather than a number."),
        nextButton()
      ) else <.p()
    )(Challenge.textColumn(
      <.div(^.cls := "card",
        <.div(^.cls := "card-header", "Exercises"),
        <.div(^.cls := "card-body",
          <.ol(
            <.li(
              <.p("Here we have a number and an AND mask. Mask everything in the lower four bits to zero, letting the upper four bits come through."),
              <.p(Binary.showBinary(0x7e, 8, showHex=true, showDecimal=false), <("code")(" & ")),
              <.p(toggle1, <("code")(" = ")),
              <.p(Binary.showBinary(toggle1.number & 0x7e, 8, showHex=true, showDecimal=false)),
              if (toggle1.number == 0xf0) <.p("Complete") else ""
            ),
            <.li(
              <.p("Here we have a number and an OR mask. Mask everything in the upper four bits to one, letting the lower four bits come through."),
              <.p(Binary.showBinary(0x6b, 8, showHex=true, showDecimal=false), <("code")(" | ")),
              <.p(toggle2, <("code")(" = ")),
              <.p(Binary.showBinary(toggle2.number | 0x6b, 8, showHex=true, showDecimal=false)),
              if (toggle2.number == 0xf0) <.p("Complete") else ""
            )
          )
        )

      ),
    ))
  )

}
