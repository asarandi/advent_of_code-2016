import scala.io.Source
import scala.collection.mutable
object Main {
  def main(args: Array[String]): Unit = {
    val fromLowHigh =
      "^(\\w+ \\d+) gives low to (\\w+ \\d+) and high to (\\w+ \\d+)$".r
    val valueGoesTo = "^value (\\d+) goes to (\\w+ \\d+)$".r

    val fromTo: mutable.Map[String, (String, String)] = mutable.Map()
    val values: mutable.Map[String, Array[Int]] = mutable.Map()

    def append(to: String, i: Int): Unit = {
      values(to) = if (!values.contains(to)) Array(i) else values(to) :+ i
    }

    Source.fromFile("input.txt").getLines.foreach { line =>
      line match {
        case fromLowHigh(from, low, high) => {
          fromTo(from) = (low, high)
        }
        case valueGoesTo(value, to) => append(to, value.toInt)
      }
    }

    var p1 = ""
    var p2 = 0

    def f(k: String): Int =
      if (values.contains(k) && values(k).length > 0) values(k)(0) else 0

    while ((p1 == "") || (p2 == 0)) {
      for ((k, v) <- values) {
        if (v.length == 2) {
          values(k) = Array()
          val sorted = v.sortWith(_ < _)
          p1 = if (sorted.sameElements(Array(17, 61))) k else p1
          val (lo, hi) = fromTo(k)
          append(lo, sorted(0))
          append(hi, sorted(1))
          p2 = f("output 0") * f("output 1") * f("output 2")
        }
      }
    }
    println(s"part 1: $p1")
    println(s"part 2: $p2")
  }
}
