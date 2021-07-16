object Main {
  def main(args: Array[String]): Unit = {

    def strxor(s: String): String = {
      var b = s.getBytes
      for (i <- 0 until b.length) b(i) = ((b(i) & 255) ^ 1).toByte
      return String(b)
    }
    val dragon = (s: String) => s + "0" + strxor(s.reverse)

    def checksum(s: String): String = {
      val tr = (s: String) => {
        s match {
          case "11" | "00" => "1"
          case "01" | "10" => "0"
        }
      }
      var w = s
      while (w.length % 2 == 0) {
        w = (0 to (w.length - 1) / 2)
          .map(i => w.slice(i * 2, i * 2 + 2))
          .map(tr(_))
          .mkString
      }
      return w
    }

    def puzzle(s: String, n: Int): String = {
      var w = s
      while (w.length < n) {
        w = dragon(w)
      }
      return checksum(w.slice(0, n))
    }

    val input = "01111010110010011"
    println(s"part 1: ${puzzle(input, 272)}")
    println(s"part 2: ${puzzle(input, 35651584)}")
  }
}
