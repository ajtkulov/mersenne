package mersenne

import scala.collection.mutable.ArrayBuffer

case class Mersenne(p : Int) {
  def next(x : Int) : Int = (x * x - 2 + p) % p

  def seq(g : Int) : Seq[Int] = {
    var elem = g
    val res = ArrayBuffer[Int]()
    val set = scala.collection.mutable.Set[Int]()
    while (!set.contains(elem)) {
      set.add(elem)
      res.append(elem)
      elem = next(elem)
    }

    res
  }

  def nonCycledSeq(values : Seq[Int]) : Boolean = {
    values.last != 2
  }

  def nonCycledGenerators() : Seq[Int] = {
    (0 to p - 1).filter(x => nonCycledSeq(seq(x)))
  }
}

object Helper {
  def interest(p : Int, size : Int): Seq[Seq[Int]] = {
    val m = Mersenne(p)
    val set = m.nonCycledGenerators()
    set.map(x=>m.seq(x)).filter(x => x.length == size)
  }
}