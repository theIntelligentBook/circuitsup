package com.wbillingsley.wren

import com.wbillingsley.wren.Orientation.East
import org.scalatest.funspec.AnyFunSpec

class ConstraintTest extends AnyFunSpec {

  describe("Constraint propagator") {

    it("should resolve equation constraints") {
      val i = new Value("A", Some(3d -> UserSet))
      val r = new Value("Ohm", Some(100d -> UserSet))
      val v = new Value("V", None)

      val p = new ConstraintPropagator(Seq(EquationConstraint("Ohm's Law", v, Seq(i, r), () => i * r)))

      p.resolve()

      assert(v.value.contains(300d))

    }

    it("should re-resolve equation constraints with updated dependencies") {
      val i = new Value("A", Some(3d -> UserSet))
      val r = new Value("Ohm", Some(100d -> UserSet))
      val v = new Value("V", None)
      val p = ConstraintPropagator(Seq(EquationConstraint("Ohm's Law", v, Seq(i, r), () => i * r)))

      p.resolve()
      i.content = Some(4d -> UserSet)

      val updated = p.resolve().map(_._1)

      assert(updated == Seq(v) && v.value.contains(400d))
    }

    it("should resolve complex sets of constraints") {

      import Wire._

      val adder = FullAdder((0,0))
      val a = new Terminal((0,0))
      val b = new Terminal((0,0))
      val c = new Terminal((0,0))
      val wires = Seq(
        (a -> adder.ta).wire,
        (b -> adder.tb).wire,
        (c -> adder.tcin).wire
      )
      val p = ConstraintPropagator(a.constraints ++ b.constraints ++ c.constraints ++ adder.constraints ++ wires.flatMap(_.constraints))

      a.potential.content = Some(5d -> UserSet)
      b.potential.content = Some(5d -> UserSet)
      c.potential.content = Some(5d -> UserSet)

      val set = p.resolve().map(_._1)

      assert(set.contains(adder.tr.potential) && adder.tr.potential.value.contains(5d))

    }

    it("should re-resolve complex sets of constraints") {

      import Wire._

      val adder = FullAdder((0,0))
      val a = new Terminal((0,0))
      val b = new Terminal((0,0))
      val c = new Terminal((0,0))
      val wires = Seq(
        (a -> adder.ta).wire,
        (b -> adder.tb).wire,
        (c -> adder.tcin).wire
      )
      val p = ConstraintPropagator(a.constraints ++ b.constraints ++ c.constraints ++ adder.constraints ++ wires.flatMap(_.constraints))

      a.potential.content = Some(5d -> UserSet)
      b.potential.content = Some(5d -> UserSet)
      c.potential.content = Some(5d -> UserSet)
      p.resolve()

      a.potential.content = Some(0d -> UserSet)
      val set = p.resolve().map(_._1)

      assert(set.contains(adder.tr.potential) && adder.tr.potential.value.contains(0d))

    }

  }




}
