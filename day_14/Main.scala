import scala.math.max
import java.security.MessageDigest
object Main {
  def main(args: Array[String]): Unit = {
    val puzzle: String = "zpqevtbw" //"abc"

    val hex = "0123456789abcdef"
    val hexb = hex.getBytes

    val md5 = MessageDigest.getInstance("MD5")

    def hash1(s: String): String = {
      md5.digest(s.getBytes).map("%02x".format(_)).mkString
    }

    def hash2(s: String): String = {
      def btos(b: Array[Byte]): Array[Byte] = {
        val res: Array[Byte] = Array.fill(b.length * 2) { 0 }
        for (i <- 0 until b.length) {
          val v = b(i) & 255
          res(i * 2) = hexb(v >> 4)
          res(i * 2 + 1) = hexb(v & 15)
        }
        return res
      }
      var b = btos(md5.digest(s.getBytes))
      for (_ <- 0 until 2016) b = btos(md5.digest(b))
      return String(b)
    }

    val trips = hex.map(c => s"$c" * 3)
    def first(s: String): String = {
      return (0 to s.length - 3)
        .map(i => s.slice(i, i + 3))
        .find(trips.contains(_))
        .getOrElse("---")
    }

    val quints = hex.map(c => s"$c" * 5)
    def quint(s: String): String = {
      val c = (0 to s.length - 5)
        .map(i => s.slice(i, i + 5))
        .find(quints.contains(_))
        .getOrElse("_")(0)
      return s"$c" * 3
    }

    var arr1: Array[String] = Array()
    var ind1: Array[Int] = Array()

    var arr2: Array[String] = Array()
    var ind2: Array[Int] = Array()

    var i: Int = 0
    while (ind1.length < 64 || ind2.length < 64) {
      val s = s"$puzzle$i"
      val h = hash1(s)
      arr1 :+= h

      val ti = quint(h)
      if (ti != "___") {
        ind1 ++= (max(0, i - 1001) until i).toArray.filter(j =>
          first(arr1(j)) == ti
        )
        //println(s"ind1: ${ind1.length}")
      }

      val hh = hash2(s)
      arr2 :+= hh

      val tj = quint(hh)
      if (tj != "___") {
        ind2 ++= (max(0, i - 1001) until i).toArray.filter(j =>
          first(arr2(j)) == tj
        )
        //println(s"ind2: ${ind2.length}")
      }

      i += 1
    }
    ind1 = ind1.sorted
    println(s"part 1: ${ind1(63)}")
    ind2 = ind2.sorted
    println(s"part 2: ${ind2(63)}")
  }
}
