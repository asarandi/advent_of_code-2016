import scala.collection.mutable.SortedSet
object Main {
  def main(args: Array[String]): Unit = {
    val N = 3017957
    var a = (1 to N).toArray

    while (a.length > 1) {
      var b: SortedSet[Int] = SortedSet()
      b ++= a
      for (i <- 0 to a.length - 1) {
        if (b.contains(a(i))) {
          b -= a((i + 1) % a.length)
        }
      }
      a = b.toArray
    }

    println(s"part 1: ${a(0)}")
  }
}

// TODO: rewrite
