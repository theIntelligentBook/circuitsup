package circuitsup

import com.wbillingsley.veautiful.html.Attacher
import org.scalajs.dom

object Main {

  val scaleChallengesToWindow:Boolean = {
    !dom.window.location.search.contains("scale=off")
  }

  def main(args:Array[String]): Unit = {
    val root = Attacher.newRoot(dom.document.getElementById("render-here"))
    root.render(Router)
  }

}
