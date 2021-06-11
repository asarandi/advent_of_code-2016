import scala.io.Source
import scala.collection.mutable

object Main {
  def main(args: Array[String]) = {
    if (args.length != 1) {
      println("expecting 1 argument")
      System.exit(1)
    }
    val re = """((?:\w+)+),?""".r
    var p1, p2: Int = 0
    for (line <- Source.fromFile(args(0)).getLines.map(_.trim)) {
      val tokens: Array[String] = re.findAllIn(line).map(_.toString).toArray
      val checksum = tokens(tokens.length - 1)
      val sectorId = tokens(tokens.length - 2).toInt
      var decoded: String = ""
      var counts: mutable.Map[Char, Int] = mutable.Map()
      for (i <- 0 to (tokens.length - 3)) {
        for (c <- tokens(i)) {
          counts(c) = if (counts.contains(c)) counts(c) + 1 else 1
          decoded += (((c - 'a' + sectorId) % 26) + 'a').toChar
        }
        decoded += " "
      }
      val sorted = counts.toList.sortWith(_._1 < _._1).sortWith(_._2 > _._2)
      var ok: Boolean = true
      for (i <- checksum.indices)
        ok &= (checksum(i) == sorted(i)(0))
      p1 = if (ok) p1 + sectorId else p1
      p2 = if (decoded == "northpole object storage ") sectorId else p2
    }
    println(s"part 1: $p1")
    println(s"part 2: $p2")
  }
}
