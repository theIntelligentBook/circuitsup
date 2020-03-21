package com.wbillingsley.wren

import org.scalatest._

class ConstraintTest extends FunSpec {

  describe("Constraint propagator") {

    it("should resolve equation constraints") {
      val i = new Value("A", Some(3d -> UserSet))
      val r = new Value("Ohm", Some(100d -> UserSet))
      val v = new Value("V", None)

      val p = new ConstraintPropagator(Seq(EquationConstraint("Ohm's Law", v, Seq(i, r), () => i * r)))

      p.resolve()

      assert(v.value.contains(300d))

    }


  }

}
