import scala.io.StdIn
import scala.collection.immutable.Map

object main {

  type Digits = List[Array[Int]]

  def main(args: Array[String]): Unit = {

    def countAtIndex(digits: Digits, index: Int): Map[Int, Int] = {
      var map = Map[Int, Int](0 -> 0, 1 -> 0)
      for (array <- digits) {
        val v = array(index)
        map = map.updatedWith(v)(existing => existing match {
          case Some(x) => Some(x + 1)
          case _ => None
        })
      }
      map
    }

    def combineDigits(digits: Digits)(fn: Map[Int, Int] => Int): Array[Int] = {
      val l = digits.head.length
      val a = Array.fill(l)(0)
      for (i <- 0 until l) {
        val counts = countAtIndex(digits, i)
        a(i) = fn(counts)
      }
      a
    }

    def mostCommon(digits: Digits): Array[Int] = {
      combineDigits(digits)(counts => if (counts(0) > counts(1)) 0 else 1)
    }

    def leastCommon(digits: Digits): Array[Int] = {
      combineDigits(digits)(counts => if (counts(0) < counts(1)) 0 else 1)
    }

    def digitToInt(digit: Array[Int]): Int = Integer.parseInt(digit.mkString(""), 2)

    val digits = io.Source.stdin.getLines().map(_.trim.split("").map(_.toInt).toArray).toList

    val mostDigits = mostCommon(digits)
    val leastDigits = leastCommon(digits)

    val mostValue = digitToInt(mostDigits)
    val leastValue = digitToInt(leastDigits)

    println(mostValue * leastValue)
  }
}
