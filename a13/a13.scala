
import scala.io.StdIn

class Board(coords: List[(Int, Int)], folds: List[(String, Int)]) {
  var w = coords.map(_._1).max + 1
  var h = coords.map(_._2).max + 1
  val points = Array.ofDim[Boolean](w, h)

  coords.foreach({ case (x, y) => points(x)(y) = true })

  def processAllFolds(): Unit = {
    folds.foreach(processFold)
  }

  def processFold(fold: (String, Int)): Unit = {
    val (coord, value) = fold
    if (coord == "x") {
      val x = value
      for (y <- 0 until h) {
        for (dx <- 1 to Math.min(x, w - 1 - x)) {
          try {
            points(x - dx)(y) = points(x - dx)(y) || points(x + dx)(y)
          } catch {
            case e => { println(s"x = $x, dx = $dx, y = $y, w = $w "); throw e }
          }
        }
      }
      w = x
    } else if (coord == "y") {
      val y = value
      for (x <- 0 until w) {
        for (dy <- 1 to Math.min(y, h - 1 - y)) {
          points(x)(y - dy) = points(x)(y - dy) || points(x)(y + dy)
        }
      }
      h = y
    }
  }

  def printBoard(): Unit = {
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        if (points(x)(y)) {
          print("#")
        } else {
          print(".")
        }
      }
      println
    }
  }

  def countDots(): Int = {
    var count = 0
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        if (points(x)(y)) {
          count += 1
        }
      }
    }
    count
  }
}

object main {

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList

    val (pointLines, foldLines) = lines.span(l => l.nonEmpty)

    val coords = pointLines.map(l => l.split(",").toList.map(_.toInt)).map({ case List(x, y) => (x, y) })
    
    val regexp = """fold along (\w)=(\d+)""".r
    val folds = foldLines.filter(l => l.nonEmpty).map({
      case regexp(coord, value) => (coord, value.toInt)
    })

    println(coords)
    println(folds)

    val board = new Board(coords, folds)
    board.processFold(folds.head)
    println(board.countDots())

    val board2 = new Board(coords, folds)
    board.processAllFolds()
    board.printBoard()
  }
}
