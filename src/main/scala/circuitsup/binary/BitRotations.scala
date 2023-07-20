package circuitsup.binary

import circuitsup.Common
import circuitsup.binary.BinaryTopic.{nextButton, onCompletionUpdate}
import circuitsup.templates.ExerciseStage
import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, ^}
import com.wbillingsley.veautiful.doctacular.Challenge
import com.wbillingsley.veautiful.doctacular.Challenge.Complete
import com.wbillingsley.wren.BinaryShifter
import org.scalajs.dom.{Element, Node}

object BitRotations extends ExerciseStage {

  var completion: Challenge.Completion = Challenge.Open

  val shifter1 = new BinaryShifter(0x6e, 8, Seq(BinaryShifter.Reset, BinaryShifter.ROL))(onUpdate)
  val shifter2 = new BinaryShifter(0xA3, 8, Seq(BinaryShifter.Reset, BinaryShifter.ROR))(onUpdate)

  val exercises:Seq[BitShiftExercise] = Seq(
    new BitShiftExercise(0x05, 0x7d, Seq(
      BinaryShifter.Reset, BinaryShifter.ROL, BinaryShifter.ASR, BinaryShifter.ROR,
    ))(onUpdate),
    new BitShiftExercise(0x014, 0xD3, Seq(
      BinaryShifter.Reset, BinaryShifter.ROL, BinaryShifter.ASR, BinaryShifter.ROR,
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

  override protected def render = <.div(
    Challenge.textAndEx(
      Common.marked(
        """
          |# Rotations
          |
          |Another possibility when doing shifts is to set the byte shfiting *in* at one end to be the byte that shifted
          |*out* at the other end. This produces a *rotation* of the byte.
          |
          |Below, here's a byte that gives you a rotate left (`ROL`) operation. Notice that if you rotate it left
          |eight times, you're back where you started.
          |""".stripMargin
      ),
      <.p(shifter1),
      Common.marked(
        """
          |And below, here's a byte that gives you a rotate right (`ROR`) operation. Notice that if you rotate it right
          |eight times, you're back where you started.
          |""".stripMargin),
      <.p(shifter2)
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
        <.p("Well done. Roll on, the next section!"),
        nextButton()
      ) else <.p()
    ))
  )

}
