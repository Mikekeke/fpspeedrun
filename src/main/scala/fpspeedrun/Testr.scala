package fpspeedrun
import fpspeedrun.syntax.ord._

object Testr extends App {
  println(Ratio(1,3) <=> Ratio(4,5))
  println(Ratio(1,2) <=> Ratio(3,6))
  println(Ratio(1,500) <=> Ratio(5,6))
  println(1.3 <=> 4.5)
  println(2.0 <=> 2.0)

}
