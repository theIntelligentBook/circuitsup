package com.wbillingsley.wren

sealed trait Provenance
case object UserSet extends Provenance
case object QuestionSet extends Provenance
case object Unknown extends Provenance
case class Because(constraint: Constraint, values:Seq[Value]) extends Provenance

class Value(val units:String, var content:Option[(Double, Provenance)] = None) {

  def value:Option[Double] = content.map(_._1)

  def >=(d:Double):Boolean = content match {
    case Some((vv, _)) if vv >= d => true
    case _ => false
  }

  def <=(d:Double):Boolean = content match {
    case Some((vv, _)) if vv <= d => true
    case _ => false
  }

  def >(d:Double):Boolean = content match {
    case Some((vv, _)) if vv > d => true
    case _ => false
  }

  def <(d:Double):Boolean = content match {
    case Some((vv, _)) if vv < d => true
    case _ => false
  }

  def -(d:Double):Option[Double] = for {
    (my, _) <- content
  } yield my - d

  def +(v:Value):Option[Double] = {
    for {
      (my, _) <- content
      (their, _) <- v.content
    } yield my + their
  }

  def -(v:Value):Option[Double] = {
    for {
      (my, _) <- content
      (their, _) <- v.content
    } yield my - their
  }

  def *(v:Value):Option[Double] = {
    for {
      (my, _) <- content
      (their, _) <- v.content
    } yield my * their
  }

  def /(v:Value):Option[Double] = {
    for {
      (my, _) <- content
      (their, _) <- v.content
    } yield my / their
  }

  def is(v:Double):Boolean = content match {
    case Some((vv, _)) if vv == v => true
    case _ => false
  }

  def is(v:Double, epsilon:Double):Boolean = content match {
    case Some((vv, _)) if Math.abs(vv - v) <= epsilon => true
    case _ => false
  }


  def stringify:String = stringify("")

  def stringify(or:String):String = content match {
    case Some((x, _)) => stringify(x)
    case _ => or
  }

  def stringify(x:Double):String = {
    if (x == 0d) {
      s"0$units"
    } else {
      val (d, p) = prefix(x)
      f"$d%.3g$p$units"
    }
  }

  def prefix(d:Double):(Double, String) = {
    val abs = Math.abs(d)

    if (abs >= 1000000000) (d / 1000000000, "G")
    else if (abs >= 1000000) (d / 1000000, "M")
    else if (abs >= 1000) (d / 1000, "k")
    else if (abs >= 1) (d, "")
    else if (abs >= 0.001) (d * 1000, "m")
    else if (abs >= 0.000001) (d * 1000000, "μ")
    else if (abs >= 0.000000001) (d * 1000000000, "n")
    else (abs, "")
  }


}

trait Constraint {

  def name:String

  def calculable:Boolean

  def values:Seq[Value]

  def calculate():Seq[Value]

  def failed:Boolean

  def satisfied:Boolean = values.forall(_.content.nonEmpty) && !failed

}

case class EqualityConstraint(name:String, values:Seq[Value]) extends Constraint {

  override def calculable: Boolean = values.exists(_.content.nonEmpty)

  override def calculate(): Seq[Value] = {
    values.find(_.content.nonEmpty) match {
      case Some(set) =>
        for {
          v <- values.filter(_.content.isEmpty)
        } yield {
          v.content = set.content.map({ case (vv, _) => (vv, Because(this, Seq(v))) })
          v
        }
      case _ => Seq.empty
    }
  }

  override def failed: Boolean = {
    val set = values.filter(_.content.nonEmpty).map(_.content.map(_._1))
    // Error if there are two values that differ by more than 1%
    set.zip(set.tail).exists{ case (Some(x), Some(y)) => Math.abs(x - y) > (x + y) / 100 }
  }

}


case class SumConstraint(name:String, values:Seq[Value], result:Double, tolerance:Double = 0.01) extends Constraint {

  override def calculable: Boolean = values.count(_.content.isEmpty) == 1

  def withinTolerance(a:Double, b:Double):Boolean = Math.abs(a / b) <= tolerance

  override def calculate(): Seq[Value] = {
    if (calculable) {
      val s = (for {
        v <- values
        (num, _) <- v.content
      } yield num).sum

      for {
        v <- values if v.content.isEmpty
      } yield {
        v.content = Some((result - s, Because(this, values.filter(_.content.isEmpty))))
        v
      }
    } else Seq.empty

  }

  override def failed: Boolean = {
    values.forall(_.content.nonEmpty) && {
      !withinTolerance(result, (for {
        v <- values
        (num, _) <- v.content
      } yield num).sum)
    }
  }

}

case class EquationConstraint(name:String, eqs:Seq[(Value, () => Option[Double])], tolerance:Double = 0.01) extends Constraint {

  def values = eqs.map(_._1)

  override def calculable: Boolean = eqs.exists({ case (v, eq) =>
    v.content.isEmpty && eq().nonEmpty
  })

  def withinTolerance(a:Double, b:Double):Boolean = Math.abs(a / b) <= tolerance

  override def calculate(): Seq[Value] = {
    if (calculable) {
      for {
        (v, eq) <- eqs if v.content.isEmpty
        newVal <- eq()
      } yield {
        v.content = Some(newVal -> Because(this, values.filter(_.content.nonEmpty)))
        v
      }
    } else Seq.empty

  }

  override def failed: Boolean = {
    //TODO: fixme
    false
  }

}


case class ConstraintPropagator(constraints:Seq[Constraint]) {

  def clearCalculations():Unit = {
    for {
      c <- constraints
      v <- c.values
      (_, Because(_, _)) <- v.content
    } {
      v.content = None
    }
  }

  def canStep:Boolean = constraints.exists({ c =>
    c.values.exists(_.content.isEmpty) && c.calculable
  })

  def step():Seq[Value] = {
    for {
      c <- constraints if c.values.exists(_.content.isEmpty) && c.calculable
      v <- c.calculate()
    } yield v
  }

  def resolve():Unit = {
    while (canStep) step()
  }

  def violated:Seq[Constraint] = constraints.filter(_.failed)

}
