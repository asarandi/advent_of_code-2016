object Main {
  def main(args: Array[String]): Unit = {
    val n = 9 * 282 // input
    var i = 2
    while (i < n) i = i << 2 | 2
    println(s"part 1: ${i - n}")
  }
}
