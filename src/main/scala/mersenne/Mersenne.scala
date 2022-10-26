package mersenne

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ArrayBuffer
import MathUtils._


object Prime {
  def isPrime(n: Int): Boolean = {
    val sq = (Math.sqrt(n) + 1).toInt
    !(2 to sq).exists(div => n % div == 0)
  }
}


object Jakobi {
  def jab(a: Int, b: Int): Int = {
    (a, b) match {
      case (a1, b1) if a == 0 => 0
      case (a1, b1) if a == 1 => 1
      case (a1, b1) if a == 2 && (b % 8 == 1 || b % 8 == 7) => 1
      case (a1, b1) if a == 2 => -1
      case (a1, b1) if a % 4 == 0 => jab(a / 4, b)
      case (a1, b1) if a % 2 == 0 && (b % 8 == 1 || b % 8 == 7) => jab(a / 2, b)
      case (a1, b1) if a % 2 == 0 => -jab(a / 2, b)
      case (a1, b1) => (if (a % 4 == 3 && b % 4 == 3) -1 else 1) * jab(b % a, a)
    }


  }


}

object MathUtils {

  implicit class Div(value: Int) {
    implicit def %%(p: Int): Int = {
      val t = (value % p)
      if (t < 0) {
        t + p
      } else {
        t
      }
    }

    implicit def pow(s: Int, p: Int): Int = {
      require(s > 0)
      var r = 1
      for (i <- 1 to s) {
        r = (r * value) %% p
      }
      r
    }
  }

}

case class Mersenne(p: Int, shift: Int = 2) {
  //  def next(x : Int) : Int = (x * x + x - shift + p) % p
  def next(x: Int): Int = (x * x) %% p

  def seq(g: Int): Seq[Int] = {
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

  def nonCycledSeq(values: Seq[Int], last: Int = 2): Boolean = {
    values.last != last
  }

  def nonCycledGenerators(last: Int = 2): Seq[Int] = {
    (0 to p - 1).filter(x => nonCycledSeq(seq(x), last))
  }

  def toDot(fileName: String): Unit = {
    val strs: IndexedSeq[String] = for (i <- 0 to p - 1) yield {
      s"     ${i} -> ${next(i)};"
    }
    val res =
      s"""
         |digraph graphname {
         |${strs.mkString("\n")}
         | }
  """.stripMargin

    FileUtils.write(fileName, Iterator.single(res))
  }
}

case class MersenneF(p: Int, next: Int => Int) {
  def toDot(fileName: String): Unit = {
    val strs: IndexedSeq[String] = for (i <- 0 to p - 1) yield {
      s"     ${i} -> ${next(i)};"
    }
    val res =
      s"""
         |digraph graphname {
         |${strs.mkString("\n")}
         | }
  """.stripMargin

    FileUtils.write(fileName, Iterator.single(res))
  }
}

object Helper {
  def interest(p: Int, size: Int): Seq[Seq[Int]] = {
    val m = Mersenne(p)
    val set = m.nonCycledGenerators()
    set.map(x => m.seq(x)).filter(x => x.length == size)
  }

  def inter2(p: Int, s: Int, last: Int): Unit = {
    val m = Mersenne(p, s)
    val nonCycle = m.nonCycledGenerators(last)
    (1 to 126).filter(x => !nonCycle.contains(x)).map(x => m.seq(x)).foreach(println)
  }

  def inter3(p: Int, s: Int, last: Int): Unit = {
    val m = Mersenne(p, s)
    val nonCycle = m.nonCycledGenerators(last)
    nonCycle.map(x => m.seq(x)).foreach(println)
  }

  def lastCommon(p: Int, s: Int): (Int, Int) = {
    val m = Mersenne(p, s)
    (0 to p - 1).par.map(x => m.seq(x).last).groupBy(x => x).map(x => (x._2.size, x._1)).toArray.sortBy(x => x._1).reverse.take(1)(0)
  }

  def bestLastCommon(p: Int): (Int, Int) = {
    (0 to p - 1).par.map(x => lastCommon(p, x)).maxBy(x => x._1)
  }

  def allFiles(p: Int): Unit = {
    for (i <- 0 to p - 1) {
      val m = Mersenne(p, i)
      m.toDot(s"${p}_${i}.dot")
    }
  }

  def files(p: Int, shift: Int): Unit = {
    val m = Mersenne(p, shift)
    m.toDot(s"${p}_${shift}.dot")
  }
}