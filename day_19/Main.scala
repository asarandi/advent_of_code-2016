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

    var p2, s: Int = 2
    for (i <- 0 until 20) {
      val k: Int = math.pow(3, i).toInt + 1
      s = if (k <= N) k else s
    }
    val d = N - s
    if (d <= s - 2) {
      p2 = d
    } else {
      p2 = (N - (s * 2 - 1)) * 2 + s
    }

    println(s"part 2: ${p2 + 1}")

  }
}

// TODO: rewrite
