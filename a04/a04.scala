import scala.io.StdIn

object main {

  type Board = Array[Array[Int]]

  def printBoard(board: Board): Unit = {
    for (line <- board) {
      println(line.mkString(" "))
    }
  }

  def printBoards(boards: List[Board]): Unit = {
    for (board <- boards) {
      printBoard(board)
      println()
    }
  }

  def wholeRowMarked(drawn: Set[Int], board: Board, rowIndex: Int): Boolean = {
    val res = board(rowIndex).iterator.forall(v => drawn.contains(v))
    res
  }

  def wholeColumnMarked(drawn: Set[Int], board: Board, columnIndex: Int): Boolean = {
    val res = board.map(row => row(columnIndex)).iterator.forall(v => drawn.contains(v))
    res
  }

  def wins(drawn: Set[Int], board: Board): Boolean = {
    (0 until 5).iterator.exists(index => wholeRowMarked(drawn, board, index) || wholeColumnMarked(drawn, board, index))
  }

  def winningBoardAndNumbers(drawn: List[Int], boards: List[Board]): Tuple3[Board, Set[Int], Int] = {
    for (init <- drawn.inits.toList.reverse.tail) {
      val numbers: Set[Int] = init.toSet
      val winning: Option[Board] = boards.find(b => wins(numbers, b))
      winning match {
        case Some(board) => { return Tuple3[Board, Set[Int], Int](board, numbers, init.last) }
        case _ => {}
      }
    }
    throw new Exception("No winner")
  }

  def boardValue(board: Board, numbers: Set[Int], lastCalled: Int): Long = {
    lastCalled * Array.concat(board:_*).filter(v => !numbers.contains(v)).sum
  }

  def solve1(drawn: List[Int], boards: List[Board]): Long = {
    val (board, numbers, lastCalled) = winningBoardAndNumbers(drawn, boards)
    boardValue(board, numbers, lastCalled)
  }

  def main(args: Array[String]): Unit = {

    var drawn: List[Int] = List()
    var boards = List[Board]()
    var lastBoard = Array.ofDim[Int](5, 5)
  
    for ((ln, index) <- io.Source.stdin.getLines().zipWithIndex) {
      if (index == 0) {
        drawn = ln.split(",").toList.map(_.toInt)
      } else if (((index - 1) % 6) == 0) {
        lastBoard = Array.ofDim[Int](5, 5)
        boards = boards :+ lastBoard
      } else {
        val ii = (index - 2) % 6
        lastBoard(ii) = ln.trim.split("\\s+").map(_.toInt)
      }
    }

    println(solve1(drawn, boards))
  }
}
