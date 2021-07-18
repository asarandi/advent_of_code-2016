import scala.collection.mutable.HashMap
object Main {
  def main(args: Array[String]): Unit = {
    val safe = '.'
    val trap = '^'

    val trapsTab: HashMap[(Char, Char, Char), Boolean] = HashMap(
      (trap, trap, safe) -> true,
      (safe, trap, trap) -> true,
      (trap, safe, safe) -> true,
      (safe, safe, trap) -> true
    )

    var nextRowTab: HashMap[String, String] = HashMap()
    var countsTab: HashMap[String, Int] = HashMap()

    def nextRow(currentRow: String): String = {
      if (nextRowTab.contains(currentRow)) {
        return nextRowTab(currentRow)
      }
      var res = ""
      val N = currentRow.length
      for (i <- 0 until N) {
        var L, C, R = '@'
        L = if (i == 0) safe else currentRow(i - 1)
        C = currentRow(i)
        R = if (i == N - 1) safe else currentRow(i + 1)
        res = if (trapsTab.contains((L, C, R))) res + trap else res + safe
      }
      nextRowTab.addOne(currentRow -> res)
      countsTab.addOne(res -> res.count(_ == safe))
      return res
    }

    def play(firstRow: String, totalRows: Int): Int = {
      var w = firstRow
      var res = w.count(_ == safe)
      countsTab.addOne(w -> res)
      for (_ <- 1 until totalRows) {
        w = nextRow(w)
        res += countsTab(w)
      }
      return res
    }

    val input =
      ".^^.^^^..^.^..^.^^.^^^^.^^.^^...^..^...^^^..^^...^..^^^^^^..^.^^^..^.^^^^.^^^.^...^^^.^^.^^^.^.^^.^."
    println(s"part 1: ${play(input, 40)}")
    println(s"part 2: ${play(input, 400000)}")
  }
}
