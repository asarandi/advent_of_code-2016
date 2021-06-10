import scala.io.Source

object Main {

  def main(args: Array[String]) = {

    if (args.length != 1) {
      println("expecting 1 argument: input file")
      System.exit(1)
    }

    var p1, p2: (Int, Int) = (0, 0)
    var d, n: Int = 0
    var seen: Set[(Int, Int)] = Set((0, 0))

    val step: Array[(Int, Int)] = Array((-1, 0), (0, 1), (1, 0), (0, -1))
    val dir: Map[Char, Int] = Map('L' -> 3, 'R' -> 1)
    val addTuples = (a: (Int, Int), b: (Int, Int)) => (a(0) + b(0), a(1) + b(1))
    val taxicab = (a: (Int, Int), b: (Int, Int)) => {
      val x = addTuples(a, b)
      x(0).abs + x(1).abs
    }

    for (item <- Source.fromFile(args(0)).mkString.split(',').map(_.trim)) {

      d = (d + dir(item(0))) % 4
      n = item.substring(1).toInt

      for (i <- 1 to n) {
        p1 = addTuples(p1, step(d))
        p2 = if (seen(p1) && (p2 == (0, 0))) p1 else p2
        seen += p1
      }
    }

    println(s"part 1: ${taxicab(p1, (0, 0))}")
    println(s"part 2: ${taxicab(p2, (0, 0))}")
  }
}
