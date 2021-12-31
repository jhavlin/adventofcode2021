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

    def filter(digits: Digits, fn: Map[Int, Int] => Int, position: Int = 0): Array[Int] = {
      if (digits.length == 1) {
        digits.head
      } else {
        val counts = countAtIndex(digits, position)
        val nextDigits = digits.filter((arr) => {
          val d = arr(position)
          d == fn(counts)
        })
        filter(nextDigits, fn, position + 1)
      }
    }

    def mostCommon(digits: Digits): Array[Int] = {
      filter(digits, counts => if (counts(1) >= counts(0)) 1 else 0)
    }

    def leastCommon(digits: Digits): Array[Int] = {
      filter(digits, counts => if (counts(0) <= counts(1)) 0 else 1)
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
