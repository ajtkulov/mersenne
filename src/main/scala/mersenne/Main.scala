package mersenne

object Main extends App {
  override def main(args : Array[String]) {
    val m = Mersenne(127)
    val set = m.nonCycledGenerators()
    println(set.size)
  }
}


