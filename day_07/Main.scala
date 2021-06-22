import scala.io.Source
import scala.collection.mutable.Set
object Main {
  def main(args: Array[String]) = {

    // returns true if `s` contains substring of `abba` format
    def HasAbba(s: String): Boolean = {
      for (i <- 0 until s.length - 3) {
        if (
          (s(i) != s(i + 1)) && (s(i) == s(i + 3)) && (s(i + 1) == s(i + 2))
        ) {
          return true
        }
      }
      return false
    }

    // returns a set of substrings with length 3 where i-0 matches i-2 but not i-1
    // for input "aba", `f` flag controls output: "aba" or "bab"
    def Subs(s: String, f: Boolean): Set[String] = {
      var res: Set[String] = Set()
      for (i <- 0 until s.length - 2) {
        if ((s(i) == s(i + 2)) && (s(i) != s(i + 1))) {
          if (f) {
            res.add(s"${s(i + 1)}${s(i)}${s(i + 1)}")
          } else {
            res.add(s"${s(i)}${s(i + 1)}${s(i)}")
          }
        }
      }
      return res
    }

    var p1, p2: Int = 0
    val inner = "\\[((?:\\w+)+)\\]?".r
    for (line <- Source.fromFile("input.txt").getLines) {
      var a, b: Int = 0 // part 1
      var x, z: Set[String] = Set() // part 2
      inner.findAllMatchIn(line).foreach { m =>
        a = if (HasAbba(m.group(1))) a + 1 else a
        x = x | Subs(m.group(1), false)
      }
      inner.replaceAllIn(line, "_").split("_").toList.foreach { m =>
        b = if (HasAbba(m)) b + 1 else b
        z = z | Subs(m, true)
      }
      p1 = if ((a == 0) && (b > 0)) p1 + 1 else p1
      p2 = if ((x & z).size > 0) p2 + 1 else p2
    }
    println(s"part 1: $p1")
    println(s"part 2: $p2")
  }
}
