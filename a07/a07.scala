import scala.io.StdIn

object main {

  def price(ints: Array[Int], target: Int): Long = {
    ints.map(v => (v - target).abs).sum
  }

  def price2(ints: Array[Int], target: Int): Long = {
    def aritSum(v: Int): Int = (v * (v + 1)) / 2 
    ints.map(v => aritSum((v - target).abs)).sum
  }

  def main(args: Array[String]): Unit = {
    val ints = StdIn.readLine().split(",").map(_.toInt).sorted
    
    val median1 = ints(ints.length / 2)

    println(price(ints, median1))

    println("\n---\n")

    val average = ints.sum / ints.length

    println(List(price2(ints, average), price2(ints, average - 1), price2(ints, average + 1)).min)
  }
}
