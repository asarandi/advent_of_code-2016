import scala.io.Source

object Main {
  def main(args: Array[String]) = {
    if (args.length != 1) {
      println("expecting 1 argument")
      System.exit(1)
    }
    var p1, p2, i: Int = 0
    var vert = Array.ofDim[Int](3, 3)

    val input = Source
      .fromFile(args(0))
      .getLines
      .map(_.trim)
      .map(_.split("\\s+").map(_.toInt))
      .toArray
    val isValid = (arr: Array[Int]) => {
      val sorted = arr.sortWith(_ < _)
      sorted(0) + sorted(1) > sorted(2)
    }
    for (i <- input.indices) {
      p1 = if (isValid(input(i))) p1 + 1 else p1
      for (j <- 0 to 2)
        vert(j)(i % 3) = input(i)(j)
      if (i % 3 == 2) {
        for (j <- 0 to 2)
          p2 = if (isValid(vert(j))) p2 + 1 else p2
      }
    }
    println(s"part 1: $p1")
    println(s"part 2: $p2")
  }
}
