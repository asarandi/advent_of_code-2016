import scala.io.Source
object Main {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("input.txt")
    val lines = source.getLines.toArray
    source.close()

    val cpyXY = "^cpy (-?\\w+) (\\w+)$".r
    val jnzXY = "^jnz (\\w+) (-?\\w+)$".r
    val incX = "^inc (\\w)$".r
    val decX = "^dec (\\w)$".r

    def run(r: Array[Int]): Int = {
      var registers = r.clone
      def getIndex(s: String): Int = "abcd".indexOf(s)
      def getValue(s: String): Int = {
        val i = getIndex(s)
        return if (i == -1) s.toInt else registers(i)
      }
      var pc: Int = 0
      while ((pc >= 0) && (pc < lines.length)) {
        lines(pc) match {
          case cpyXY(x, y) => {
            registers(getIndex(y)) = getValue(x)
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
          case _ =>
        }
        pc += 1
      }
      registers(0)
    }
    println(s"part 1: ${run(Array(0, 0, 0, 0))}")
    println(s"part 2: ${run(Array(0, 0, 1, 0))}")
  }
}
