import scala.io.StdIn

object main {

  def main(args: Array[String]): Unit = {
    val matrix = Array.ofDim[Int](1000, 1000)
    val regexp = """(\d+),(\d+) -> (\d+),(\d+)""".r
    val lines = io.Source.stdin.getLines().map((ln) => {
      ln match {
        case regexp(x1, y1, x2, y2) => ((x1.toInt, y1.toInt), (x2.toInt, y2.toInt))
        case _ => throw new Exception("Invalid line")
      }
    }).toList

    lines.foreach((line) => {
      val ((x1, y1), (x2, y2)) = line
      if (x1 == x2) {
        for (y <- Math.min(y1, y2) to Math.max(y1, y2)) {
          matrix(x1)(y) += 1 
        }
      } else if (y1 == y2) {
        for (x <- Math.min(x1, x2) to Math.max(x1, x2)) {
          matrix(x)(y1) += 1
        }
      } else if (x1 < x2) {
        if (y1 < y2) {
          for (y <- y1 to y2) {
            val d = y - y1
            matrix(x1 + d)(y1 + d) += 1
          }
        } else { // y2 < y1
          for (y <- y2 to y1) {
            val d = y - y2
            matrix(x1 + d)(y1 - d) += 1
          }
        }
      } else { // x2 < x1
        if (y1 < y2) {
          for (y <- y1 to y2) {
            val d = y - y1
            matrix(x1 - d)(y1 + d) += 1
          }
        } else { // y2 < y1
          for (y <- y2 to y1) {
            val d = y - y2
            matrix(x1 - d)(y1 - d) += 1
          }
        }
      }
    })

    var count = 0

    for (a <- matrix) {
      for (b <- a) {
        if (b > 1) {
          count += 1
        }
      }
    }

    println(count)
  }
}
