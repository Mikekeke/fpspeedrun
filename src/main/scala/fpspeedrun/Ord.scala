package fpspeedrun

import fpspeedrun.Ord.Compare

trait Ord[T] extends Eq[T]{
  def compare(x: T, y: T): Compare
  def ===(x: T, y: T): Boolean = compare(x,y) match {
    case Compare.EQ => true
    case _ => false
  }
}

object Ord{
  sealed trait Compare
  object Compare{
    case object LT extends Compare //less than
    case object EQ extends Compare //equals to
    case object GT extends Compare //greater than
  }

  import fpspeedrun.syntax.eq._
  implicit val doubleCompare : Ord[Double] = (x: Double, y: Double) => if (x === y) Compare.EQ
  else if (x > y) Compare.GT
  else Compare.LT
}
