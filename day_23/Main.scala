import collection.mutable.HashMap
import scala.io.Source
object Main {
  def main(args: Array[String]): Unit = {

    val cpyXY = "^cpy (-?\\w+) (\\w+)$".r
    val jnzXY = "^jnz (\\w+) (-?\\w+)$".r
    val incX = "^inc (\\w)$".r
    val decX = "^dec (\\w)$".r
    val tglX = "^tgl (\\w)$".r

    val tab: HashMap[String, String] = HashMap(
      "inc" -> "dec",
      "dec" -> "inc",
      "tgl" -> "inc",
      "cpy" -> "jnz",
      "jnz" -> "cpy"
    )

    val toggle = (s: String) => tab(s.slice(0, 3)) + s.slice(3, s.length)

    def run(instr: Array[String], regs: Array[Int]): Int = {

      val instructions = instr.clone
      val registers = regs.clone
      def getIndex(s: String): Int = "abcd".indexOf(s)
      def getValue(s: String): Int = {
        val i = getIndex(s)
        return if (i == -1) s.toInt else registers(i)
      }

      var pc: Int = 0
      while ((pc >= 0) && (pc < instructions.length)) {
//        println(
//          f"$pc%02d     ${instructions(pc)}%-10s    [${registers.mkString(", ")}]"
//        )
        instructions(pc) match {
          case cpyXY(x, y) => {
            val i = getIndex(y)
            if (i != -1) {
              registers(i) = getValue(x)
            }
          }
          case jnzXY(x, y) => {
            pc = if (getValue(x) != 0) pc + getValue(y) - 1 else pc
          }
          case incX(x) => {
            registers(getIndex(x)) += 1
          }
          case decX(x) => {
            registers(getIndex(x)) -= 1
          }
          case tglX(x) => {
            val t = getValue(x) + pc
            if ((t >= 0) && (t < instructions.length)) {
              instructions(t) = toggle(instructions(t))
            }
          }
          case _ =>
        }
        pc += 1
      }
      registers(0)
    }

    val source = Source.fromFile("input.txt")
    val input = source.getLines.toArray
    source.close()

    def factorial(n: Int): Int = if (n == 0) 1 else (n * factorial(n - 1))

    println(s"part 1: ${run(input, Array(7, 0, 0, 0))}")
    println(s"part 2: ${factorial(12) + 73 * 82}")
  }
}
