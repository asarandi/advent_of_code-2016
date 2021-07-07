import scala.io.Source
object Main {
  def main(args: Array[String]) = {
    val re = "^(.*?)\\((\\d+)x(\\d+)\\)(.*)$".r
    val input = Source.fromFile("input.txt").getLines.mkString
    def decompress(data: String, recursive: Boolean): Long = {
      var res: Long = 0
      var s: String = data
      while (re.findFirstMatchIn(s).nonEmpty) {
        val m = re.findFirstMatchIn(s).get
        res += m.group(1).length
        val p = m.group(2).toInt
        val q = m.group(3).toInt
        val t = m.group(4)
        if (recursive) {
          res += decompress(t.slice(0, p), true) * q
        } else {
          res += p * q
        }
        s = t.slice(p, t.length)
      }
      return res + s.length
    }
    println(s"part 1: ${decompress(input, false)}")
    println(s"part 2: ${decompress(input, true)}")
  }
}
