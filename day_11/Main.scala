import scala.collection.mutable.Set
import scala.collection.mutable.HashMap
import scala.collection.mutable.PriorityQueue
object Main {
  def main(args: Array[String]): Unit = {
    type Node = (Int, Int, Int, Int, Int)

    def nodeToArray(n: Node): Array[Int] = n.toArray.map(_.asInstanceOf[Int])
    def arrayToNode(a: Array[Int]): Node = (a(0), a(1), a(2), a(3), a(4))

    def numSetBits(upTo: Int): Array[Int] = {
      val res = new Array[Int](upTo)
      for (i <- 0 until upTo) {
        var (ct, n) = (0, i)
        while (n > 0) {
          ct += 1
          n &= n - 1
        }
        res(i) = ct
      }
      return res
    }

    val bits = numSetBits(1 << 14)
    def heuristic(n: Node): Int = bits(n(1)) * 3 + bits(n(2)) * 2 + bits(n(3))

    def isConflict(i: Int): Boolean = {
      val evenBits = 5461 // 0b01010101010101
      val G = i & (evenBits << 1)
      val M = (i & evenBits) << 1
      return (G != 0) && (M != 0) && ((G & M) != M)
    }

    def isValid(n: Node): Boolean =
      !(isConflict(n(1)) || isConflict(n(2)) ||
        isConflict(n(3)) || isConflict(n(4)))

    def isGoal(n: Node): Boolean = (n(1) == 0) && (n(2) == 0) && (n(3) == 0)

    val trans: Array[Array[Int]] =
      Array(Array.empty[Int], Array(2), Array(1, 3), Array(2, 4), Array(3))
    def children(node: Node): Set[Node] = {
      var res: Set[Node] = Set()
      val curr = node(0)
      trans(curr).foreach { next =>
        var child = nodeToArray(node)
        child(0) = next
        for (i <- 0 until 14) {
          if (((1 << i) & child(curr)) != 0) {
            var one = child.clone
            one(curr) ^= (1 << i)
            one(next) ^= (1 << i)
            val n = arrayToNode(one)
            if (isValid(n)) {
              res += n
            }
            for (j <- (i + 1) until 14) {
              if (((1 << j) & child(curr)) != 0) {
                var two = one.clone
                two(curr) ^= (1 << j)
                two(next) ^= (1 << j)
                val n = arrayToNode(two)
                if (isValid(n)) {
                  res += n
                }
              }
            }
          }
        }
      }

      return res
    }

    def search(start: Node): Int = {
      var counter: Int = 0
      val openSet: HashMap[Node, (Int, Int)] = HashMap()
      openSet += (start -> (0, 0))
      val closedSet: Set[Node] = Set()
      val pq: PriorityQueue[(Int, Int, Int, Node, Node)] =
        PriorityQueue()(Ordering[(Int, Int)].on(x => (-x._1, -x._2)))
      pq.enqueue((0, counter, 0, start, (0, 0, 0, 0, 0)))
      counter += 1
      while (!pq.isEmpty) {
        val (f, c, g_, node, parent) = pq.dequeue()
        if (isGoal(node)) {
          return g_
        }
        if (!closedSet.contains(node)) {
          children(node).foreach { ch =>
            var G = g_ + 1
            var H = 0
            if (!closedSet.contains(ch)) {
              var proceed = true
              if (openSet.contains(ch)) {
                val (oG, oH) = openSet(ch)
                if (oG <= G) {
                  proceed = false
                } else {
                  H = oH
                }
              } else {
                H = heuristic(ch)
              }
              if (proceed) {
                openSet += (ch -> (G, H))
                pq.enqueue((G + H, counter, G, ch, node))
                counter += 1
              }
            }
          }
        }
        closedSet += node
      }
      return -1
    }

    val sample = (1, 5, 8, 2, 0)
    val inputOne = (1, 15, 752, 256, 0)
    val inputTwo = (1, 15375, 752, 256, 0)

    println(s"sample: ${search(sample)}")
    println(s"part 1: ${search(inputOne)}")
    println(s"part 2: ${search(inputTwo)}")
  }
}
