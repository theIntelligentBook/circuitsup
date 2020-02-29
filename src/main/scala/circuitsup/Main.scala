package circuitsup

import com.wbillingsley.veautiful.html.Attacher
import org.scalajs.dom

object Main {

  val scaleChallengesToWindow:Boolean = {
    !dom.window.location.search.contains("scale=off")
  }

  def main(args:Array[String]): Unit = {
    val node = dom.document.getElementById("render-here")
    node.innerHTML = ""
    val root = Attacher.newRoot(node)
    root.render(Router)
  }

}
