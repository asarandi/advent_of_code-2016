import scala.io.Source

object Main {
  def main(args: Array[String]) = {
    if (args.length != 1) {
      println("expecting one argument")
      System.exit(1)
    }

    var pos1: (Int, Int) = (2, 2)
    var pos2: (Int, Int) = (2, 0)
    var ans1, ans2: String = ""

    val moves: Map[Char, (Int, Int)] =
      Map('U' -> (-1, 0), 'R' -> (0, 1), 'D' -> (1, 0), 'L' -> (0, -1))

    val alphabet = "_123456789ABCD"
    val keypad1 = Array(
      Array(0, 0, 0, 0, 0),
      Array(0, 1, 2, 3, 0),
      Array(0, 4, 5, 6, 0),
      Array(0, 7, 8, 9, 0),
      Array(0, 0, 0, 0, 0)
    )

    val keypad2 = Array(
      Array(0, 0, 1, 0, 0),
      Array(0, 2, 3, 4, 0),
      Array(5, 6, 7, 8, 9),
      Array(0, 10, 11, 12, 0),
      Array(0, 0, 13, 0, 0)
    )

    val addTuples = (a: (Int, Int), b: (Int, Int)) => (a(0) + b(0), a(1) + b(1))
    val isValid = (pos: (Int, Int), keypad: Array[Array[Int]]) =>
      (0 <= pos(0)) && (pos(0) < 5) &&
        (0 <= pos(1)) && (pos(1) < 5) &&
        (keypad(pos(0))(pos(1)) != 0)

    for (line <- Source.fromFile(args(0)).getLines().map(_.trim)) {
      for (c <- line) {

        val move1: (Int, Int) = addTuples(pos1, moves(c))
        pos1 = if (isValid(move1, keypad1)) move1 else pos1

        val move2: (Int, Int) = addTuples(pos2, moves(c))
        pos2 = if (isValid(move2, keypad2)) move2 else pos2

      }
      ans1 += alphabet(keypad1(pos1(0))(pos1(1)))
      ans2 += alphabet(keypad2(pos2(0))(pos2(1)))
    }
    println(s"part 1: $ans1")
    println(s"part 2: $ans2")
  }
}
