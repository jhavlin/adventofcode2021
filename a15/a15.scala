import scala.io.StdIn

class Board(risks: Array[Array[Int]]) {
  val h = risks.length
  val w = risks(0).length

  val done = Array.ofDim[Boolean](h, w)
  val dist = Array.ofDim[Long](h, w)
  
  forArray((y, x) => dist(y)(x) = Long.MaxValue)
  dist(0)(0) = 0
  var resolved = 0

  while (resolved < w * h) {
    val (minDist, y, x) = findMinInNotDone()
    done(y)(x) = true
    resolved += 1

    println(s"solved $resolved / ${w * h}, ${resolved.toDouble / (w * h).toDouble}")

    for ((dy, dx) <- List((0, -1), (-1, 0), (1, 0), (0, 1))) {
      if (y + dy >= 0 && y + dy < h && dx + x >= 0 && dx + x < w && !done(y + dy)(x + dx)) {
        if (minDist + risks(y + dy)(x + dx) < dist(y + dy)(x + dx)) {
          dist(y + dy)(x + dx) = minDist + risks(y + dy)(x + dx)
        }
      }
    }
  }

  def findMinInNotDone(): (Long, Int, Int) = {
    var min = Long.MaxValue
    var minCoords = (-1, -1)
    forArray((y, x) => {
      if (!done(y)(x) && dist(y)(x) < min) {
        min = dist(y)(x)
        minCoords = (y, x)
      }
    })
    (min, minCoords._1, minCoords._2)
  }

  def printBoard(): Unit = {
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        print(risks(y)(x))
      }
      println()
    }
  }

  def forArray(fn: (Int, Int) => Unit): Unit = {
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        fn(y, x)
      }
    }
  }

  def endDist: Long = dist(h - 1)(w - 1)
}

object main {

  def tileArray(orig: Array[Array[Int]]): Array[Array[Int]] = {
    val h = orig.length
    val w = orig(0).length
    val res = Array.ofDim[Int](h * 5, w * 5)
    for (my <- 0 until 5) {
      for (mx <- 0 until 5) {
        for (y <- 0 until h) {
          for (x <- 0 until w) {
            res(my * h + y)(mx * w + x) = if (my == 0 && mx == 0) {
              orig(y)(x)
            } else if (my == 0) {
              var raw = res(my * h + y)((mx - 1) * w + x) + 1
              if (raw > 9) {
                1
              } else {
                raw
              }
            } else {
              var raw = res((my - 1) * h + y)(mx * w + x) + 1
              if (raw > 9) {
                1
              } else {
                raw
              }
            }
          }
        }       
      }
    }
    res
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList
    val risks = lines.toArray.map(line => line.toArray.map(_.asDigit))
    val board = new Board(risks)
    board.printBoard()
    println(board.endDist)

    println()

    val tiled = tileArray(risks)
    val board2 = new Board(tiled)
    board2.printBoard()
    println(board2.endDist)
  }
}
