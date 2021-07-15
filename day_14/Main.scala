import scala.math.max
import java.security.MessageDigest
object Main {
  def main(args: Array[String]): Unit = {
    val md5 = MessageDigest.getInstance("MD5")
    val hash = (s: String) =>
      md5.digest(s.getBytes).map("%02x".format(_)).mkString
    val puzzle: String = "zpqevtbw" //"abc"

    val trips = "0123456789abcdef".map(c => s"$c" * 3)
    def first(s: String): String = {
      return (0 to s.length - 3)
        .map(i => s.slice(i, i + 3))
        .find(trips.contains(_))
        .getOrElse("---")
    }

    val quints = "0123456789abcdef".map(c => s"$c" * 5)
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
      val h = hash(s)
      arr1 :+= h

      val ti = quint(h)
      if (ti != "___") {
        ind1 ++= (max(0, i - 1001) until i).toArray.filter(j =>
          first(arr1(j)) == ti
        )
        //println(s"ind1: ${ind1.length}")
      }

      var hh = h
      for (_ <- 0 until 2016) hh = hash(hh)
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
