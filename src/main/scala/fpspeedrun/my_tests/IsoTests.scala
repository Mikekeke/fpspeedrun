package fpspeedrun.my_tests

object syntax {

  object semigroup {

    implicit class SemiOps[T](a: T) {
      def combine(other: T)(implicit sg: Semigroup[T]) = sg.combine(a, other)

      def |+|(other: T)(implicit sg: Semigroup[T]) = combine(other)
    }

  }

}

final case class Sum[T](v: T) extends AnyVal

object Sum {

  implicit def sumIso[T]: Iso[T, Sum[T]] = new Iso[T, Sum[T]] {
    override def wrap(a: T): Sum[T] = Sum(a)

    override def unwrap(b: Sum[T]): T = b.v
  }

  implicit def sumIntSemi: Semigroup[Sum[Int]] = (a: Sum[Int], b: Sum[Int]) => Sum(a.v + b.v)
}

final case class Prod[T](v: T) extends AnyVal

object Prod {
  implicit def prodIso[T]: Iso[T, Prod[T]] = new Iso[T, Prod[T]] {
    override def wrap(a: T): Prod[T] = Prod(a)

    override def unwrap(b: Prod[T]): T = b.v
  }

  implicit def prodIntSemi: Semigroup[Prod[Int]] = (a: Prod[Int], b: Prod[Int]) => Prod(a.v * b.v)
}

trait Iso[T, U] {
  def wrap(a: T): U

  def unwrap(b: U): T
}

trait Semigroup[T] {
  def combine(a: T, b: T): T
}

object Semigroup {

  import syntax.semigroup._

  implicit val semigString: Semigroup[String] = (a: String, b: String) => a + b

  def combineList[T: Semigroup](xs: List[T]): Option[T] = xs.reduceOption(_ |+| _)

  // ver.1
  def combineListViaV1[T, U: Semigroup](xs: List[T]) (implicit iso: Iso[T, U]): Option[T] =
    xs.reduceOption((l, r) => iso.unwrap(iso.wrap(l) |+| iso.wrap(r)))

  // ver.2
  def combineListViaV2[T, U[_]](xs: List[T]) (implicit iso: Iso[T, U[T]], sg: Semigroup[U[T]]): Option[T] =
    xs.reduceOption((l, r) => iso.unwrap(iso.wrap(l) |+| iso.wrap(r)))

  // v3. with partial type application pattern
  def combineListViaV3[U[_]] = new CombineListVia[U]
  class CombineListVia[U[_]] {
    def apply[T](xs: List[T]) (implicit iso: Iso[T, U[T]], sg: Semigroup[U[T]]): Option[T] =
      xs.reduceOption((l, r) => iso.unwrap(iso.wrap(l) |+| iso.wrap(r)))
  }
}


object IsoTests {

  import syntax.semigroup._
  import Sum._
  import Prod._
  import Semigroup._

  def main(args: Array[String]): Unit = {
    val res = Sum(4) |+| Sum(10)
    println(res)

    val res1 = Semigroup.combineList(List("Y", "O", "L", "O"))
    println(s"res1 = $res1")

    val testList = (2 to 5).toList
    println(testList)
    val resSum = Semigroup.combineListViaV1[Int, Sum[Int]](testList) // ver.1
    println(s"resSumVer1 = $resSum")
    val resProdVer2 = Semigroup.combineListViaV2[Int, Prod](testList) // ver.2
    println(s"resProdVer2 = $resProdVer2")
    val resProdVer3 = Semigroup.combineListViaV3[Prod](testList) // ver.3 with partial type application pattern
    println(s"resProdVer3 = $resProdVer3")
  }
}
