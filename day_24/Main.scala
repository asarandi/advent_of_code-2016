import collection.mutable.HashMap
import collection.mutable.Queue
import scala.io.Source
object Main {
  def main(args: Array[String]): Unit = {

    val src = Source.fromFile("input.txt")
    val input = src.getLines.toArray
    src.close

    type vec = (Int, Int)
    val (n, m) = (input.length, input(0).length)

    def isBounds(yx: vec): Boolean = {
      val (y, x) = yx
      return (y >= 0) && (y < n) && (x >= 0) && (x < m)
    }

    val locations: HashMap[Char, vec] = HashMap()
    for (y <- 0 until n) {
      for (x <- 0 until m) {
        val c = input(y)(x)
        if ("0123456789".indexOf(c) != -1) {
          locations(c) = (y, x)
        }
      }
    }

    def moves(yx: vec): Array[vec] = {
      val (y, x) = yx
      val URDL: Array[vec] = Array((-1, 0), (0, 1), (1, 0), (0, -1))
      return URDL
        .map((i, j) => (y + i, x + j))
        .filter(p => isBounds(p))
        .filter((i, j) => input(i)(j) != '#')
    }

    def bfs(from: vec, to: vec): Int = {
      var queue: Queue[(Int, vec)] = Queue((0, from))
      var seen: HashMap[vec, Boolean] = HashMap(from -> true)
      while (!queue.isEmpty) {
        val (g, curr) = queue.dequeue()
        if (curr == to) {
          return g
        }
        moves(curr).foreach { move =>
          if (!seen.contains(move)) {
            seen(move) = true
            queue.append((g + 1, move))
          }
        }
      }
      return -1
    }

    val taxicab = (v: vec, w: vec) => (v(0) - w(0)).abs + (v(1) - w(1)).abs
    var dist: HashMap[Char, HashMap[Char, Int]] = HashMap()

    locations.foreach { (src, _) => dist(src) = HashMap() }
    locations.foreach { (src, from) =>
      locations.foreach { (dst, to) =>
        var d = bfs(from, to)
        dist(dst)(src) = d
        dist(src)(dst) = d
      }
    }

    def calc(s: String): Int = {
      var res = 0
      (1 until s.length).foreach { i =>
        val (a, b) = (s(i - 1), s(i))
        res += dist(a)(b)
      }
      return res
    }

    var p1, p2 = (1 << 63) - 1
    def permute(arr: Array[Char], L: Int, R: Int): Unit = {
      if (L == R) {
        val d1 = calc("0" + arr.mkString)
        p1 = if (d1 < p1) d1 else p1
        val d2 = calc("0" + arr.mkString + "0")
        p2 = if (d2 < p2) d2 else p2
      } else {
        for (i <- L until R) {
          var t = arr(i); arr(i) = arr(L); arr(L) = t
          permute(arr, L + 1, R)
          t = arr(i); arr(i) = arr(L); arr(L) = t
        }
      }
    }

    val arr = (1 until locations.size).mkString.toArray
    permute(arr, 0, arr.length)

    println(s"part 1: $p1")
    println(s"part 2: $p2")
  }
}
