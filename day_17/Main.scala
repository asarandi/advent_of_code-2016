import java.security.MessageDigest
import scala.collection.mutable.Queue
object Main {
  def main(args: Array[String]): Unit = {
    type vec = (Int, Int, String)

    def hexString(in: Array[Byte]): String = {
      val hexb = "0123456789abcdef".getBytes
      val out: Array[Byte] = Array.fill(in.length * 2) { 0 }
      for (i <- 0 until in.length) {
        val v = in(i) & 255
        out(i * 2) = hexb(v >> 4)
        out(i * 2 + 1) = hexb(v & 15)
      }
      return String(out)
    }
    val md5 = MessageDigest.getInstance("MD5")
    val hash = (s: String) => hexString(md5.digest(s.getBytes))

    val addVectors = (v: vec, w: vec) => (v(0) + w(0), v(1) + w(1), v(2) + w(2))
    val isBounds = (v: vec) => v(0) >= 0 && v(0) < 4 && v(1) >= 0 && v(1) < 4
    val isGoal = (v: vec) => v(0) == 3 && v(1) == 3

    def search(input: String, longest: Boolean): String = {
      var queue: Queue[vec] = Queue((0, 0, input))
      val steps: Array[vec] =
        Array((-1, 0, "U"), (1, 0, "D"), (0, -1, "L"), (0, 1, "R"))
      var maxPath: Int = 0
      while (!queue.isEmpty) {
        val curr = queue.dequeue()
        if (isGoal(curr)) {
          return curr(2).substring(input.length) // part 1
        }
        val h = hash(curr(2))
        (0 until 4)
          .filter(i => h(i) > 'a')
          .map(i => addVectors(curr, steps(i)))
          .filter(v => isBounds(v))
          .foreach { move =>
            if (longest && isGoal(move)) {
              val path = move(2).length - input.length
              maxPath = if (path > maxPath) path else maxPath
            } else {
              queue.enqueue(move)
            }
          }
      }
      return maxPath.toString // part 2
    }

    println(s"part 1: ${search("njfxhljp", false)}")
    println(s"part 2: ${search("njfxhljp", true)}")
  }
}
