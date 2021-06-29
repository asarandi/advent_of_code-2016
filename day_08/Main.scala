import Array.ofDim
import scala.io.Source
object Main {
  def main(args: Array[String]): Unit = {
    val reColumn = "^rotate\\ column\\ x=(\\d+) by (\\d+)$".r
    val reRow = "^rotate\\ row\\ y=(\\d+) by (\\d+)$".r
    val reRect = "^rect\\ (\\d+)x(\\d+)$".r
    var g = ofDim[Int](6, 50)
    var t = ofDim[Int](6)

    val source = Source.fromFile("input.txt")
    for (line <- source.getLines()) {
      line match {
        case reColumn(xs, qs) => {
          val x = xs.toInt
          val q = qs.toInt
          for (j <- 0 until 6) t(j) = g(j)(x)
          t = t.slice(6 - q, 6) ++ t.slice(0, 6 - q)
          for (j <- 0 until 6) g(j)(x) = t(j)
        }
        case reRow(ys, qs) => {
          val y = ys.toInt
          val q = qs.toInt
          g(y) = g(y).slice(50 - q, 50) ++ g(y).slice(0, 50 - q)
        }
        case reRect(xs, ys) => {
          for (j <- 0 until ys.toInt)
            for (i <- 0 until xs.toInt)
              g(j)(i) = 1
        }
      }
    }
    source.close()
    println(s"part 1: ${g.map(_.sum).sum}")
    println(s"part 2:\n")
    println(g.map(_.mkString("").replace('0', ' ')).mkString("\n"))
  }
}
