import scala.io.StdIn

class Board(lines: Array[String]) {
  val w = lines(0).length
  val h = lines.length

  val directions = List((-1, 0), (1, 0), (0, 1), (0, -1))

  def at(x: Int, y: Int): Int = {
    lines(y).charAt(x).asDigit
  }

  def neighbors(x: Int, y: Int): List[Int] = {
    var res = List[Int]()
    for (dir <- directions) {
      val (dx, dy) = dir
      val nx = x + dx
      val ny = y + dy
      if (nx >= 0 && nx < w && ny >= 0 && ny < h) {
        res = res :+ at(nx, ny)
      }
    }
    res
  }

  def solve1(): Int = {
    var sum = 0
    for (x <- 0 until w) {
      for (y <- 0 until h) {
        val value = at(x, y)
        val next = neighbors(x, y)
        if (next.forall(n => n > value)) {
          sum += value + 1
        }
      }
    }
    sum
  }

  def flood(x: Int, y: Int, visited: Array[Array[Boolean]]): Long = {
    if (x >= 0 && x < w && y >= 0 && y < h && at(x, y) < 9 && !visited(x)(y)) {
      visited(x)(y) = true
      1 + flood(x + 1, y, visited) + flood(x - 1, y, visited) + flood(x, y + 1, visited) + flood(x, y - 1, visited)
    } else {
      0
    }
  }

  def solve2(): Long = {
    val visited = Array.ofDim[Boolean](w, h)
    var sizes = List[Long]()
    for (x <- 0 until w) {
      for (y <- 0 until h) {
        val value = at(x, y)
        if (value < 9 && !visited(x)(y)) {
          sizes = sizes :+ flood(x, y, visited)
        }
      }
    }
    sizes.sorted.reverse.take(3).product
  }
}

object main {

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toArray
    val board = new Board(lines)

    println(board.solve1())
    println(board.solve2())
  }
}
