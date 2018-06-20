package fpspeedrun

import fpspeedrun.Ord.Compare

trait Ord[T] extends Eq[T]{
  def compare(x: T, y: T): Compare
}

object Ord{
  sealed trait Compare
  object Compare{
    case object LT extends Compare //less than
    case object EQ extends Compare //equals to
    case object GT extends Compare //greater than
  }

  implicit val doubleCompare : Ord[Double] = new Ord[Double] {
    import fpspeedrun.syntax.eq._
    import fpspeedrun.Eq.doubleEq
    override def compare(x: Double, y: Double): Compare =
      if (x === y) Compare.EQ
      else if (x > y) Compare.GT
      else Compare.LT

//    override def ===(x: Double, y: Double): Boolean = throw new Exception("kek")// can I tell it to use existing instances somehow?
    override def ===(x: Double, y: Double): Boolean = doubleEq.===(x,y)
  }
}


// class Lolable a where
// lol :: a -> String
//
// instance Lolable Int where
// lol n = (show n) ++ " lold"
//
// class Lolable a => Kekable a where
// kek :: a -> String
//
// instance Kekable Int where
// kek n = (show n) ++ " kekd"
//
// funFun :: Kekable a => a -> String
// funFun a = (lol a ) ++ " & " ++ (kek a)