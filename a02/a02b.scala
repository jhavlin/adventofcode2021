import scala.io.StdIn

object main {

  def main(args: Array[String]): Unit = {
    
    var horizontal: Long = 0
    var depth: Long = 0
    var aim: Long = 0

    val command = """(\w+) (\d+)""".r

    for (ln <- io.Source.stdin.getLines()) {
      ln match {
        case command(cmd, value) => {
          val v: Long = value.toLong
          cmd match {
            case "forward" => { horizontal += v; depth += (aim * v) }
            case "up" => { aim -= v; }
            case "down" => { aim += v}
          }
        }
      }
    }

    println(horizontal * depth)
  }
}
