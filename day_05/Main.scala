import java.security.MessageDigest
object Main {
  def main(args: Array[String]) = {
    val md5: MessageDigest = MessageDigest.getInstance("MD5")
    val puzzle: String = "uqwqemis"
    val hexDigest: (Int) => String = (i: Int) =>
      md5.digest((puzzle + i.toString).getBytes).map("%02x" format _).mkString
    val isMatch: (Int) => Boolean = (i: Int) => {
      val d = md5.digest((puzzle + i.toString).getBytes)
      (d(0) == 0) && (d(1) == 0) && ((d(2) & 240) == 0)
    }
    var p1, p2: Array[Char] = Array(0, 0, 0, 0, 0, 0, 0, 0)
    var q1, q2: Int = 0
    var i: Int = 0
    while (q2 < 8) {
      if (isMatch(i)) {
        val s: String = hexDigest(i)
        if (q1 < 8) {
          p1(q1) = s(5)
          q1 += 1
        }
        val n: Int = (s(5) - '0').toInt
        if ((0 <= n) && (n <= 7) && (p2(n) == 0)) {
          p2(n) = s(6)
          q2 += 1
        }
      }
      i += 1
    }
    println(s"part 1: ${p1.mkString}")
    println(s"part 2: ${p2.mkString}")
  }
}
