import scala.io.StdIn

object main {

  def increases(count: Int, prevValue: Int, values: List[Int]): Int =
    values match {
      case v :: rs =>
        if (v > prevValue) increases(count + 1, v, rs)
        else increases(count, v, rs)
      case Nil => count
    }

  def main(args: Array[String]): Unit = {
    val ints = io.Source.stdin.getLines().map(_.toInt).toList
    val res = ints match {
      case v :: rs => increases(0, v, rs)
      case Nil     => 0
    }
    println(res)

    var slidingSums = ints.sliding(3).map(_.sum).toList
    val res2 = slidingSums match {
      case v :: rs => increases(0, v, rs)
      case Nil     => 0
    }
    println(res2)
  }
}
