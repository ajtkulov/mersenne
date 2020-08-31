package mersenne

import MathUtils._

/*
  a[0] + a[1] * x + a[2] * x ^ 2 + ... mod m
 */
case class Poly(a: List[Int], mod: Int) {
  def at(x: Int): Int = {
    a.zip(Iterator.iterate(1)(t => (t * x) %% mod).take(a.length).toList).map(q => q._1 * q._2).sum %% mod
  }
}

object Bijection {
  def isBijection(p: Poly, m: Int): Boolean = {
    (0 to m - 1).map(x => p.at(x)).distinct.size == m
  }

}
