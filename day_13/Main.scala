import scala.collection.mutable.Queue
import scala.collection.mutable.HashMap
object Main {
  def main(args: Array[String]): Unit = {
    type vec2 = (Int, Int)

    def setBits(x: Int): Int = {
      var n: Int = x
      var ct: Int = 0
      while (n > 0) {
        n &= n - 1
        ct += 1
      }
      return ct
    }

    def isValid(pos: vec2, puzzle: Int): Boolean = {
      val (y, x) = pos
      (y >= 0) && (x >= 0) &&
      ((setBits(x * x + 3 * x + 2 * x * y + y + y * y + puzzle) & 1) == 0)
    }

    def moves(curr: vec2, puzzle: Int): Array[vec2] = {
      val URDL: Array[vec2] = Array((-1, 0), (0, 1), (1, 0), (0, -1))
      var res: Array[vec2] = Array()
      URDL.foreach { step =>
        val move = (curr(0) + step(0), curr(1) + step(1))
        res = if (isValid(move, puzzle)) res :+ move else res
      }
      return res
    }

    def search(finish: vec2, puzzle: Int): (Int, Int) = {
      var queue: Queue[vec2] = Queue((1, 1))
      var seen: HashMap[vec2, Int] = HashMap((1, 1) -> 0)

      while (!queue.isEmpty) {
        val curr = queue.dequeue()
        val G = seen(curr)

        if (curr == finish) {
          return (G, seen.filterInPlace((k, v) => v <= 50).size)
        }
        moves(curr, puzzle).foreach { move =>
          if (!seen.contains(move)) {
            queue.enqueue(move)
            seen.addOne(move -> (G + 1))
          }
        }
      }
      return (-1, -1)
    }

    val (p1, p2) = search((39, 31), 1362)
    println(s"part 1: $p1")
    println(s"part 2: $p2")

  }
}
