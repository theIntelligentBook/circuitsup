package circuitsup

import com.wbillingsley.veautiful.<

object Intro {

  def page = Common.layout(<.div(
    <.h1("Circuits Up!"),
    <.p(
      """
        | This is going to grow into a suite of outreach materials that introduce students to computer architecture,
        | from circuits up...
      """.stripMargin)
  ))

}
