import scala.io.StdIn

class Board(input: Array[String]) {
  val h = input.length
  val w = input(0).length
  var values = Array.ofDim[Int](w, h)
  val flashValue = 10
  var flushes = 0
  for ((line, row) <- input.zipWithIndex) {
    for ((c, column) <- line.zipWithIndex) {
      values(column)(row) = c.asDigit
    }
  }

  def printBoard(array: Array[Array[Int]] = values): Unit = {
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        print(array(x)(y))
      }
      println()
    }
  }

  def nextStep(): Long = {
    val nextValues = Array.ofDim[Int](w, h)
    copy(values, nextValues)
    increaseAll(nextValues)

    val flushed = Array.ofDim[Boolean](w, h)
    var run = true
    while (run) {
      run = false
      forCoords((x, y) => {
        if (nextValues(x)(y) >= flashValue && !flushed(x)(y)) {
          forNeighbors(x, y, (nx, ny) => {
            nextValues(nx)(ny) += 1
            run = true
          })
          flushed(x)(y) = true
        }
      })
    }

    var flushesInStep = 0
    forCoords((x, y) => {
      if (nextValues(x)(y) > 9) {
        flushes += 1
        flushesInStep += 1
        nextValues(x)(y) = 0
      }
    })

    values = nextValues
    flushesInStep
  }

  def increaseAll(array: Array[Array[Int]]): Unit = {
    forCoords((x, y) => { array(x)(y) += 1 })
  }

  def copy(source: Array[Array[Int]], target: Array[Array[Int]]): Unit = {
    forCoords((x, y) => { target(x)(y) = source(x)(y) })
  }

  def forCoords(fn: (Int, Int) => Unit): Unit = {
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        fn(x, y)
      }
    }
  }

  def forNeighbors(x: Int, y: Int, fn: (Int, Int) => Unit): Unit = {
    for (dx <- -1 to 1) {
      for (dy <- -1 to 1) {
        if (dx != 0 || dy != 0) {
          val rx = x + dx
          val ry = y + dy
          if (rx >= 0 && rx < w && ry >= 0 && ry < h) {
            fn(rx, ry)
          }
        }
      }
    }
  }
}

object main {

  def solve1(input: Array[String]): Long = {
    val board = new Board(input)
    
    for (step <- 1 to 100) {
      //println(s"After step $step")
      board.nextStep()
      // board.printBoard()
      // println()
    }
    board.flushes
  }

  def solve2(input: Array[String]): Long = {
    val board = new Board(input)
    
    var step = 0
    var found = false
    
    while (!found) {
      step += 1
      val flashes = board.nextStep()
      if (flashes == board.w * board.h) {
        found = true
      }
    }
    
    step
  }

  def main(args: Array[String]): Unit = {
    val input = io.Source.stdin.getLines().toArray
    println(solve1(input))
    println(solve2(input))
  }
}
