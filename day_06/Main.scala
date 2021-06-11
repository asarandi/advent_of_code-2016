import scala.io.Source
import scala.collection.mutable
object Main {
  def main(args: Array[String]) = {
    if (args.length != 1) {
      println("expecting 1 argument")
      System.exit(1)
    }
    var counts: Array[mutable.Map[Char, Int]] = Array()
    for (line <- Source.fromFile(args(0)).getLines.map(_.trim)) {
      for (i <- line.indices) {
        if (counts.length < i + 1)
          counts = counts :+ mutable.Map()
        val c: Char = line(i)
        counts(i)(c) = if (counts(i).contains(c)) (counts(i)(c) + 1) else 1
      }
    }
    var p1, p2: String = ""
    counts.foreach((mm: mutable.Map[Char, Int]) => {
      p1 += mm.toSeq.sortWith(_._2 > _._2)(0)(0)
      p2 += mm.toSeq.sortWith(_._2 < _._2)(0)(0)
    })
    println(s"part 1: $p1")
    println(s"part 2: $p2")
  }
}
