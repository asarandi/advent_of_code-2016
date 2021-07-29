import scala.io.Source
object Main {
  def main(args: Array[String]): Unit = {

    def scramble(arr: Array[Char], steps: Array[String]): Array[Char] = {

      def rotri(a: Array[Char], n: Int): Array[Char] = {
        var s = a.clone
        for (_ <- 0 until n) {
          s = s.slice(s.length - 1, s.length) ++ s.slice(0, s.length - 1)
        }
        return s
      }

      val moveXY = "move position (\\d+) to position (\\d+)".r
      val reverseXY = "reverse positions (\\d+) through (\\d+)".r
      val rotatePos = "rotate based on position of letter (.)".r
      val rotateLeft = "rotate left (\\d+) step.*".r
      val rotateRight = "rotate right (\\d+) step.*".r
      val swapAB = "swap letter (.) with letter (.)".r
      val swapXY = "swap position (\\d+) with position (\\d+)".r

      var s = arr.clone

      steps.foreach { step =>
        step match {
          case moveXY(x, y) => {
            val c = s.slice(x.toInt, x.toInt + 1)
            s = s.slice(0, x.toInt) ++ s.slice(x.toInt + 1, s.length)
            s = s.slice(0, y.toInt) ++ c ++ s.slice(y.toInt, s.length)
          }

          case reverseXY(x, y) =>
            s = s.slice(0, x.toInt) ++
              s.slice(x.toInt, y.toInt + 1).reverse ++
              s.slice(y.toInt + 1, s.length)

          case rotatePos(a) => {
            val i = s.indexOf(a(0))
            val n = 1 + i + (if (i >= 4) 1 else 0)
            s = rotri(s, n)
          }

          case rotateLeft(x) => {
            for (_ <- 0 until x.toInt) s = s.slice(1, s.length) ++ s.slice(0, 1)
          }

          case rotateRight(x) => s = rotri(s, x.toInt)

          case swapAB(a, b) =>
            s = s.map(c => if (c == a(0)) b(0) else if (c == b(0)) a(0) else c)
          case swapXY(x, y) => {
            val t = s(x.toInt)
            s(x.toInt) = s(y.toInt)
            s(y.toInt) = t
          }
          case _ =>
        }
      }
      return s
    }

    val src = Source.fromFile("input.txt")
    val moves = src.getLines.toArray
    src.close

    var input = "abcdefgh".toArray
    println(s"part 1: ${scramble(input, moves).mkString}")

    /*
     * part 2
     */

    val search = "fbgdceah".toArray
    var found = false
    var result = ""

    def permute(arr: Array[Char], L: Int, R: Int): Unit = {
      if (found)
        return
      if ((L == R) && (scramble(arr, moves).sameElements(search))) {
        result = arr.mkString
        found = true
      } else {
        for (i <- L until R) {
          var t = arr(i); arr(i) = arr(L); arr(L) = t;
          permute(arr, L + 1, R)
          t = arr(i); arr(i) = arr(L); arr(L) = t;
        }
      }
    }

    permute(input, 0, input.length)
    println(s"part 2: $result")

  }
}
