package fpspeedrun

import syntax.eq._

import scala.annotation.tailrec

trait Eq[T] {
  def ===(x: T, y: T): Boolean
}

object Eq {
//  def fromEquals[T]: Eq[T] = new Eq[T] {
//    override def ===(x: T, y: T): Boolean = x == y
//  }
//  def fromEquals[T]: Eq[T] = (x: T, y: T) => x == y
  def fromEquals[T]: Eq[T] = _ == _

  implicit val intEq: Eq[Int] = fromEquals
  implicit val longEq: Eq[Long] = fromEquals
  implicit val doubleEq: Eq[Double] = fromEquals

  /** простой вариант */
  implicit def vectorEq[A: Eq]: Eq[Vector[A]] =
    (xs, ys) => xs.view.zip(ys).forall { case (x, y) => x === y }


  /** я у мамы оптимизатор */
  implicit def listEq[A: Eq]: Eq[List[A]] = {
    @tailrec def go(xs: List[A], ys: List[A]): Boolean =
      xs match {
        case Nil => ys.isEmpty
        case x :: xt => ys match {
          case Nil => false
          case y :: yt => x === y && go(xt, yt)
        }
      }

    go
  }
}
