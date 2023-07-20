package circuitsup.binary

import com.wbillingsley.veautiful.DiffNode
import com.wbillingsley.veautiful.html.{<, VHtmlComponent, ^}
import com.wbillingsley.wren.{Binary, BinaryShifter}
import org.scalajs.dom.{Element, Node}

class BitShiftExercise(initial:Int, target:Int, ops:Seq[String])(onUpdate: () => Unit) extends VHtmlComponent{
  
  val shifter = new BinaryShifter(initial, 8, ops)(move)

  def isComplete:Boolean = (0xff & shifter.number) == (target & 0xff)

  def reset():Unit = {
    shifter.reset()
  }

  def move():Unit = {
    onUpdate()
  }
  
  override protected def render = {
    <.p(
      <.p(s"Here's a target bit pattern:"),
      <.p(Binary.showBinary(target, 8)),
      <.p("Use the operations below to make the byte below match the target:"),
      <.p(shifter),
      <.p(if (isComplete) "Complete!" else "")
    )
  }
}
