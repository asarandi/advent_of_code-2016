object Main {
  def main(args: Array[String]): Unit = {

    def puzzle(m: Array[Int], p: Array[Int]): Int = {

      var mods = m.clone
      var curr = p.clone
      var goal = p.clone

      val offset: Int = 1

      def tick(): Unit = {
        for (i <- 0 until curr.length) {
          curr(i) = (curr(i) + 1) % mods(i)
        }
      }

      for (i <- 0 until goal.length) {
        val n = mods(i) - ((offset + i) % mods(i));
        goal(i) = n % mods(i);
      }

      var t: Int = offset
      while (!curr.sameElements(goal)) {
        tick()
        t += 1
      }
      return t
    }

    val p1 = puzzle(Array(17, 19, 7, 13, 5, 3), Array(5, 8, 1, 7, 1, 0))
    println(s"part 1: $p1")

    val p2 = puzzle(Array(17, 19, 7, 13, 5, 3, 11), Array(5, 8, 1, 7, 1, 0, 0))
    println(s"part 2: $p2")
  }
}
