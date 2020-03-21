package com.wbillingsley.wren

import scala.collection.mutable

sealed trait Provenance
case object UserSet extends Provenance
case object QuestionSet extends Provenance
case object Unknown extends Provenance
case class Because(constraint: Constraint, values:Seq[(Value, Int)]) extends Provenance

class Value(val units:String, initial:Option[(Double, Provenance)] = None, val name:Option[String] = None) {

  private var _version:Int = 0

  def version = _version

  private var _content:Option[(Double, Provenance)] = initial

  def content:Option[(Double, Provenance)] = _content

  def content_=(c:Option[(Double, Provenance)]):Unit = {
    // We only update our version if the value has actually changed
    if (c.map(_._1) != _content.map(_._1)) {
      _version += 1
    }

    // But we have to update the provenance (so that because clauses do not remain out of date, with old versions)
    _content = c
  }

  /** True if the value is only one step out of date */
  def needsCalculation:Boolean = _content match {
    case None => true
    case Some((_, Because(_, deps))) =>
      deps.exists { case (v, vversion) => v.version != vversion } &&
        deps.forall { case (v, _) => v.fresh }
    case _ => false
  }

  /** True if this value and all its dependencies are current */
  def fresh:Boolean = _content match {
    case Some((_, Because(_, deps))) => deps.forall { case (v, version) => v.version == version && v.fresh }
    case None => false
    case _ => true
  }

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

  /** Whether it can be calculated (regardless of if it needs to be) */
  def calculable:Boolean

  /** The values it can produce */
  def values:Seq[Value]

  /** Perform the calculation, returning updated values */
  def calculate():Seq[Value]

  def failed:Boolean

  def satisfied:Boolean = values.forall(_.content.nonEmpty) && !failed

}

case class EqualityConstraint(name:String, values:Seq[Value]) extends Constraint {

  override def calculable: Boolean = values.exists(_.fresh)

  override def calculate(): Seq[Value] = {
    values.find(_.fresh) match {
      case Some(set) =>
        for {
          v <- values if v.needsCalculation
        } yield {
          v.content = set.value.map({ d => (d, Because(this, Seq(set -> set.version))) })
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

  override def calculable: Boolean = values.count(_.needsCalculation) == 1

  def withinTolerance(a:Double, b:Double):Boolean = Math.abs(a / b) <= tolerance

  override def calculate(): Seq[Value] = {
    if (calculable) {
      val s = (for {
        v <- values if !v.needsCalculation
        num <- v.value
      } yield num).sum

      for {
        v <- values if v.needsCalculation
      } yield {
        v.content = Some((result - s, Because(this, values.filterNot(_ == v).map(x => x -> x.version))))
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

case class EquationConstraint(name:String, value:Value, dependencies:Seq[Value], eq:() => Option[Double], tolerance:Double = 0.01) extends Constraint {

  def values = Seq(value)

  override def calculable: Boolean = dependencies.forall(_.fresh)

  def withinTolerance(a:Double, b:Double):Boolean = Math.abs(a / b) <= tolerance

  override def calculate(): Seq[Value] = {
    if (calculable) {
      (for {
        newVal <- eq()
      } yield {
        value.content = Some(newVal -> Because(this, dependencies.map(x => x -> x.version)))
        value
      }).toSeq
    } else Seq.empty
  }

  override def failed: Boolean = {
    //TODO: fixme
    false
  }

}

object EquationConstraint {

  def apply(name:String, eqs:Seq[(Value, () => Option[Double])]):Seq[EquationConstraint] = {
    val values = eqs.map(_._1)

    for { (v, eq) <- eqs } yield EquationConstraint(name, v, values.filterNot(_ == v), eq)
  }

}


case class ConstraintPropagator(constraints:Seq[Constraint]) {

  /** Maximum number of calculations in a single resolve() call before we assume there's a cycle and stop */
  val MAX_CALCULATIONS = 100

  case class TooManyCalculationsException(done:Seq[(Value, Option[(Double, Provenance)])]) extends RuntimeException

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
    c.values.exists(_.needsCalculation) && c.calculable
  })

  def step():Seq[Value] = {
    for {
      c <- constraints if c.values.exists(_.needsCalculation) && c.calculable
      v <- c.calculate()
    } yield v
  }

  def resolve():Seq[(Value, Option[(Double, Provenance)])] = {
    val done = mutable.Buffer.empty[(Value, Option[(Double, Provenance)])]

    while (canStep) {
      done.appendAll(step().map(x => x -> x.content))

      if (done.length > MAX_CALCULATIONS) {
        for {
          (v, why) <- done
        } {
          println(s"Set $v to $why")
        }

        throw TooManyCalculationsException(done.toSeq)
      }
    }

    done.toSeq
  }

  def violated:Seq[Constraint] = constraints.filter(_.failed)

}
