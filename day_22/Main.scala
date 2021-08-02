import collection.mutable.PriorityQueue
import collection.mutable.HashMap
import scala.io.Source
object Main {
  def main(args: Array[String]): Unit = {
    val src = Source.fromFile("input.txt")
//    val src = Source.fromFile("sample.txt")
    val data = src.getLines.toArray
    src.close

    val reLine =
      "^\\/dev\\/grid\\/node-x(\\d+)-y(\\d+)\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)%$".r

    type vec = (Int, Int, Int, Int)

    val (rows, cols) = (31, 32)
//    val (rows, cols) = (3, 3)
    val grid = Array.ofDim[vec](rows, cols)

    data.foreach { line =>
      line match {
        case reLine(x, y, size, used, avail, use) => {
          grid(y.toInt)(x.toInt) = (size.toInt, used.toInt, avail.toInt, 0)
        }
        case _ =>
      }
    }

    var (p1, blank) = (0, -1)

    for (i <- 0 until rows * cols) {
      val (size0, used0, avail0, use0) = grid(i / cols)(i % cols)
      blank = if ((blank == -1) && (used0 == 0)) i else blank
      for (j <- 0 until rows * cols) {
        val (size1, used1, avail1, use1) = grid(j / cols)(j % cols)
        p1 = if ((i != j) && (used0 != 0) && (used0 <= avail1)) p1 + 1 else p1
      }
    }

    println(s"part 1: $p1")

    def taxicab(i: Int, j: Int): Int = {
      val (y0, x0) = (i / cols, i % cols)
      val (y1, x1) = (j / cols, j % cols)
      return (y0 - y1).abs + (x0 - x1).abs
    }

    def moves(pos: Int, grid: Array[Array[vec]]): Array[Int] = {
      val (y, x) = (pos / cols, pos % cols)
      val URDL = Array[(Int, Int)]((-1, 0), (0, 1), (1, 0), (0, -1))
      return URDL
        .map(p => (p(0) + y, p(1) + x))
        .filter(p => // bounds
          ((p(0) >= 0) && (p(0) < rows) &&
            (p(1) >= 0) && (p(1) < cols))
        )
        .filter(p => // new-used <= curr-avail
          grid(p(0))(p(1))(1) <= grid(y)(x)(2)
        )
        .map(p => p(0) * cols + p(1))
    }

    def heuristic(blank: Int, goal: Int): Int = {
      return taxicab(0, goal) + taxicab(0, blank) + taxicab(blank, goal)
    }

//    type node = (f, ct, g, h, blank, goal, grid)

    type node = (Int, Int, Int, Int, Int, Int, Array[Array[vec]])
    type node2 = (Int, Int)

    var ct = 0
    val start: node = (0, ct, 0, 0, blank, cols - 1, grid)
    val start2: node2 = (blank, cols - 1)

    val pq: PriorityQueue[node] =
      PriorityQueue(start)(Ordering.by(-_(0)))

    val openSet: HashMap[node2, (Int, Int)] = HashMap()
    val closedSet: HashMap[node2, Boolean] = HashMap()

    var maxG, p2 = -1

    while (!pq.isEmpty && p2 == -1) {
      val state = pq.dequeue
      val (currF, currCt, currG, currH, currBlank, currGoal, currGrid) = state
      if (currGoal == 0) {
        p2 = currG
      }

      val n2: node2 = (currBlank, currGoal)
      if (!closedSet.contains(n2)) {
        closedSet += n2 -> true

        val (y0, x0) = (currBlank / cols, currBlank % cols)
        val (yg, xg) = (currGoal / cols, currGoal % cols)

//        if (currG > maxG) {
//          maxG = currG
//          println(s"new maxG:$maxG, pos y:$y0, x:$x0   goal y:$yg, x:$xg")
//        }

        var (size0, used0, avail0, use0) = grid(y0)(x0)

        moves(currBlank, currGrid).foreach { newBlank =>
          val newGrid = currGrid.clone
          val (y1, x1) = (newBlank / cols, newBlank % cols)
          var (size1, used1, avail1, use1) = grid(y1)(x1)
          newGrid(y1)(x1) = (size1, 0, size1, 0)
          newGrid(y0)(x0) = (size0, used0 + used1, size0 - (used0 + used1), 0)
          val newGoal = if (newBlank == currGoal) currBlank else currGoal

          val newNode2: node2 = (newBlank, newGoal)
          if (!closedSet.contains(newNode2)) {
            var newG = currG + 1
            var newH = 0
            var proceed = 1

            if (openSet.contains(newNode2)) {
              val (existingG, existingH) = openSet(newNode2)
              if (existingG <= newG) {
                proceed = 0
              } else {
                newH = existingH
              }
            } else {
              newH = heuristic(newBlank, newGoal)
            }

            if (proceed == 1) {
              openSet += newNode2 -> (newG, newH)

              ct += 1
              val newCt = ct
              val newNode: node =
                (newG + newH, newCt, newG, newH, newBlank, newGoal, newGrid)
              pq += newNode

            }
          }
        }
      }
    }
    println(s"part 2: $p2")
  }
}
