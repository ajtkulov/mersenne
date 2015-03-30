package mersenne

import scala.collection.mutable.ArrayBuffer

case class Mersenne(p : Int, shift : Int = 2) {
  def next(x : Int) : Int = (x * x - shift + p) % p

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

  def nonCycledSeq(values : Seq[Int], last : Int = 2) : Boolean = {
    values.last != last
  }

  def nonCycledGenerators(last : Int = 2) : Seq[Int] = {
    (0 to p - 1).filter(x => nonCycledSeq(seq(x), last))
  }
}

object Helper {
  def interest(p : Int, size : Int): Seq[Seq[Int]] = {
    val m = Mersenne(p)
    val set = m.nonCycledGenerators()
    set.map(x=>m.seq(x)).filter(x => x.length == size)
  }

  def inter2(p : Int, s : Int, last : Int) : Unit = {
    val m = Mersenne(p, s)
    val nonCycle = m.nonCycledGenerators(last)
    (1 to 126).filter(x => !nonCycle.contains(x)).map(x => m.seq(x)).foreach(println)
  }

  def inter3(p : Int, s : Int, last : Int) : Unit = {
    val m = Mersenne(p, s)
    val nonCycle = m.nonCycledGenerators(last)
    nonCycle.map(x => m.seq(x)).foreach(println)
  }

  def lastCommon(p : Int, s : Int) : Unit = {
    val m = Mersenne(p, s)
    (0 to p - 1).map(x => m.seq(x).last).groupBy(x => x).map(x => (x._2.size, x._1)).toArray.sortBy(x => x._1).reverse.take(5).foreach(println)
  }
}