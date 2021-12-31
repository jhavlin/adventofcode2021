import scala.io.StdIn

object main {

  def main(args: Array[String]): Unit = {
    
    var horizontal = 0
    var depth = 0

    val command = """(\w+) (\d+)""".r

    for (ln <- io.Source.stdin.getLines()) {
      ln match {
        case command(cmd, value) => {
          val v = value.toInt
          cmd match {
            case "forward" => horizontal += v
            case "up" => depth -= v
            case "down" => depth += v
          }
        }
      }
    }

    println(horizontal * depth)
  }
}
