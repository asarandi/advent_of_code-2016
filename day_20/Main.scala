import scala.io.Source
object Main {
  def main(args: Array[String]): Unit = {
    type vec = (Long, Long)
    val lohi = "^(\\d+)-(\\d+)$".r
    var data: Array[vec] = Array()
    Source.fromFile("input.txt").getLines.foreach { line =>
      line match {
        case lohi(lo, hi) => data :+= (lo.toLong, hi.toLong)
      }
    }
    data = data.sortWith(_(0) < _(0))
    var p1, p2, max: Long = 0
    for (i <- 0 until data.length) {
      val d = data(i)(0) - max - 1
      if (d > 0) {
        p1 = if (p1 == 0) max + 1 else p1
        p2 += d
      }
      max = if (data(i)(1) > max) data(i)(1) else max
    }
    println(s"part 1: $p1")
    println(s"part 2: $p2")
  }
}
