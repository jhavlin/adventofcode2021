import scala.io.StdIn

object main {

  type Neighbors = Map[String, List[String]]

  def linesToNeighbors(lines: List[String]): Neighbors = {
    var map = Map[String, List[String]]()
    for (line <- lines) {
      val List(a, b) = line.split("-").toList.map(_.trim)
      map = map.updatedWith(a) {
        case Some(list) => Some(b +: list)
        case None       => Some(List(b))
      }
      map = map.updatedWith(b) {
        case Some(list) => Some(a +: list)
        case None       => Some(List(a))
      }
    }
    map
  }

  def isBig(string: String) = string.toUpperCase == string

  def waysToEnd(current: String, path: List[String], neighbors: Neighbors): Long = {
    if (current == "end") {
      1
    } else {
      val nexts = neighbors.getOrElse(current, Nil)
      nexts.map(next => {
        if (isBig(next)) {
          waysToEnd(next, current +: path, neighbors)
        } else {
          if (path contains next) {
            0
          } else {
            waysToEnd(next, current +: path, neighbors)
          }
        }
      }).sum
    }
  }

  def solve1(neighbors: Neighbors): Long = {
    waysToEnd("start", List(), neighbors)
  }

  def waysToEnd2(current: String, path: List[String], twiceUsed: Boolean, neighbors: Neighbors): Long = {
    if (current == "end") {
      1
    } else {
      val nexts = neighbors.getOrElse(current, Nil)
      nexts.map(next => {
        if (isBig(next)) {
          waysToEnd2(next, current +: path, twiceUsed, neighbors)
        } else {
          if (path contains next) {
            if (twiceUsed || next == "start" || next == "end") {
              0
            } else {
              waysToEnd2(next, current +: path, true, neighbors)
            }
          } else {
            waysToEnd2(next, current +: path, twiceUsed, neighbors)
          }
        }
      }).sum
    }
  }

  def solve2(neighbors: Neighbors): Long = {
    waysToEnd2("start", List(), false, neighbors)
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList
    val neighbors = linesToNeighbors(lines)
    println(neighbors)
    println(solve1(neighbors))
    println(solve2(neighbors))
  }
}
