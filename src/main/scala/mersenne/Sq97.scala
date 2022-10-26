package mersenne

object Primes {
  def isPrime(n: Int): Boolean = n >= 2 && (2 to math.sqrt(n).toInt).forall(n%_ != 0)

  def iter: Iterator[Int] = Iterator.from(2).filter(isPrime)
}

case class Sq97(value: Int, maxCycleLen: Int = 2) {
  def check: Boolean = {
    val map = scala.collection.mutable.Map[Int, Int]()

    (0 until value).foreach { x =>
      map(x) = ((x.toLong * x.toLong) % value).toInt
    }


    val cycleLength = scala.collection.mutable.Map[Int, Int]()

    (2 until  value).forall { start =>
      cycle(start, map.toMap, cycleLength, scala.collection.mutable.Set(), Nil) <= maxCycleLen
    }
  }

  def checkEq: Boolean = {
    val map = scala.collection.mutable.Map[Int, Int]()

    (0 until value).foreach { x =>
      map(x) = ((x.toLong * x.toLong) % value).toInt
    }


    val cycleLength = scala.collection.mutable.Map[Int, Int]()

    (2 until  value).forall { start =>
      cycle(start, map.toMap, cycleLength, scala.collection.mutable.Set(), Nil) == maxCycleLen
    }
  }

  def cycle(start: Int, map: Map[Int, Int], cycleLength: scala.collection.mutable.Map[Int, Int], visited: scala.collection.mutable.Set[Int], path: List[Int]): Int = {
    if (cycleLength.contains(start)) {
      cycleLength(start)
    } else {
      val next = map(start)
      if (cycleLength.contains(next)) {
        cycleLength(start) = cycleLength(next)
        cycleLength(start)
      } else {
        if (visited.contains(next)) {
          val len = path.zipWithIndex.find(_._1 == next).get._2 + 1
          visited.foreach { i =>
            cycleLength(i) = len
          }
          len
        } else {
          visited.add(start)
          cycle(next, map, cycleLength, visited, start +: path)
        }
      }
    }
  }
}
