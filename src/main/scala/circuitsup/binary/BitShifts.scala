package circuitsup.binary

import circuitsup.Common
import circuitsup.binary.BinaryTopic.{nextButton, onCompletionUpdate}
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, ^}
import com.wbillingsley.veautiful.templates.Challenge
import com.wbillingsley.veautiful.templates.Challenge.Complete
import com.wbillingsley.wren.{Binary, BinaryShifter, BinaryToggle}
import org.scalajs.dom.{Element, Node}

import scala.util.Random

object BitShifts extends ExerciseStage {

  var completion: Challenge.Completion = Challenge.Open

  val shifter1 = new BinaryShifter(0x6e, 8, Seq(BinaryShifter.Reset, BinaryShifter.LSL))(onUpdate)
  val shifter2 = new BinaryShifter(0xA3, 8, Seq(BinaryShifter.Reset, BinaryShifter.ASR), signed=true)(onUpdate)
  val shifter3 = new BinaryShifter(0x75, 8, Seq(BinaryShifter.Reset, BinaryShifter.LSR))(onUpdate)

  val exercises:Seq[BitShiftExercise] = Seq(
    new BitShiftExercise(0x04, 0x7e, Seq(
      BinaryShifter.Reset, BinaryShifter.LSL, BinaryShifter.ASR, BinaryShifter.LSR,
    ))(onUpdate),
    new BitShiftExercise(0x011, 0x56, Seq(
      BinaryShifter.Reset, BinaryShifter.LSL, BinaryShifter.ASR, BinaryShifter.LSR,
    ))(onUpdate)
  )

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
          |# Bit Shifting
          |
          |Some operations can be done on individual bytes. On this page, let's try some "shifts", where we move
          |all the bits in a byte either to the left or to the right.
          |
          |If we're shifting left, we'd fill the rightmost bit with a `0`. This is called "Logical Shift Left" (`LSL`)
          |and in some programming languages is shown with the `<<` operator. Try it with the byte below. If you've
          |shifted zeroes all the way through the byte, you can reset it to the original value with the "Reset" button.
          |""".stripMargin
      ),
      <.p(shifter1),
      Common.marked(
        """
          |If we're shifting right, we have a decision to make: do we fill the leftmost bit with a zero, or with the
          |value in the sign bit (to stop negative numbers turning positive when shifted). If we preserve the sign bit,
          |the operation is called *Arithmetic Shift Right* (`ASR`) and in some programming languages is shown with the
          |`>>` operator.
          |""".stripMargin),
      <.p(shifter2),
      Common.marked(
        """
          |If we always fill with a zero, it's called *Logical Shift Right* (`LSR`) and in some
          |programming languages is shown with the `>>>` operator.
          |""".stripMargin
      ),
      <.p(shifter3),

    )(Challenge.textColumn(
      <.div(^.cls := "card",
        <.div(^.cls := "card-header", "Exercises"),
        <.div(^.cls := "card-body",
          <.ol(
            for { ex <- exercises } yield <.li(ex)
          )
        )

      ),
      if (isComplete) <.div(
        <.p("You might notice that a shift right generally halves the number, while a shift left generally doubles it."),
        nextButton()
      ) else <.p()
    ))
  )

}
