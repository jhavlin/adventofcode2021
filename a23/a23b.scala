import scala.io.StdIn

object main {

  val neighbors = Map(
    0 -> List((1, 1), (9, 2), (10, 2)),
    1 -> List((0, 1), (15, 1)),
    2 -> List((3, 1), (10, 2), (11, 2)),
    3 -> List((2, 1), (17, 1)),
    4 -> List((5, 1), (11, 2), (12, 2)),
    5 -> List((4, 1), (19, 1)),
    6 -> List((7, 1), (12, 2), (13, 2)),
    7 -> List((6, 1), (21, 1)),
    8 -> List((9, 1)),
    9 -> List((0, 2), (8, 1), (10, 2)),
    10 -> List((0, 2), (2, 2), (9, 2), (11, 2)),
    11 -> List((2, 2), (4, 2), (10, 2), (12, 2)),
    12 -> List((4, 2), (6, 2), (11, 2), (13, 2)),
    13 -> List((6, 2), (12, 2), (14, 1)),
    14 -> List((13, 1)),
    15 -> List((1, 1), (16, 1)),
    16 -> List((15, 1)),
    17 -> List((3, 1), (18, 1)),
    18 -> List((17, 1)),
    19 -> List((5, 1), (20, 1)),
    20 -> List((19, 1)),
    21 -> List((7, 1), (22, 1)),
    22 -> List((21, 1)),
  )

  val rooms = Map(
    'A' -> List(0, 1, 15, 16),
    'B' -> List(2, 3, 17, 18),
    'C' -> List(4, 5, 19, 20),
    'D' -> List(6, 7, 21, 22)
  )

  def isRoom(i: Int): Boolean = i < 8 || i > 14

  val price = Map(
    'A' -> 1,
    'B' -> 10,
    'C' -> 100,
    'D' -> 1000
  )

  def possibleMoves(state: String, index: Int): List[(Int, Int)] = {
    val c = state.charAt(index)
    // println(s"c = $c")
    var visited = Set[Int]()
    if (isRoom(index)) {
      var res = List[(Int, Int)]()
      var q = new scala.collection.mutable.Queue[(Int, Int)]()
      q.enqueue((index, 0))
      while (q.nonEmpty) {
        val (i, dist) = q.dequeue()
        // println(s"$i $dist, $visited")
        if (!visited.contains(i)) {
          if (dist > 0 && !isRoom(i)) {
            res = (i, dist) +: res
          }
          visited = visited + i
          neighbors(i).foreach({ case (n, d) => {
            if (state.charAt(n) == '.' && !visited.contains(n)) {
              q.enqueue((n, d + dist))
            }
          }})
        }
      }
      res.reverse 
    } else { // !isRoom(index)
      var res = List[(Int, Int)]()
      var q = new scala.collection.mutable.Queue[(Int, Int)]()
      q.enqueue((index, 0))
      while (q.nonEmpty) {
        val (i, dist) = q.dequeue()
        // println("current " + i + " (" + dist + ") " + q.toList + " visited " + visited)
        if (!visited.contains(i)) {
          if (isRoom(i) && rooms(c).contains(i) && rooms(c).forall(r => state.charAt(r) == '.' || state.charAt(r) == c)) {
            res = (i, dist) +: res
          }
          visited = visited + i
          neighbors(i).foreach({ case (n, d) => {
            if (state.charAt(n) == '.' && !visited.contains(n)) {
              q.enqueue((n, d + dist))
            }
          }})
        }
      }
      res.reverse
    }
  }

  def inFinalPlace(state: String, i: Int): Boolean = {
    val c = state.charAt(i)
    val finals = rooms(c)
    val List(r1, r2, r3, r4) = finals
    if (i == r4) {
      true
    } else if (i == r3 && state.charAt(r4) == c) {
      true
    } else if (i == r2 && state.charAt(r3) == c && state.charAt(r4) == c) {
      true
    } else if (i == r1 && state.charAt(r2) == c && state.charAt(r3) == c && state.charAt(r4) == c) {
      true
    } else {
      false
    }
  }

  val interesting = List("BACDBCDA", "BACD.CDA", "BA.DCCDA", "BA.BCCDA", ".ABBCCDA", ".ABBCCDD", ".ABBCC..", "AABBCCDD", "BA.D.CDA", ".ABBCCD.", "AABBCCDD", ".ABBCCD....D.A.", ".ABBCC.D")
  val veryInteresting = List(".ABBCCDD", ".ABBCC.D")

  def solve1(initialState: String): Long = {
    var states = Map(initialState -> 0L)
    val queue = new scala.collection.mutable.Queue[String]()
    queue.enqueue(initialState)
    var step = 0

    while (queue.nonEmpty) {
      step += 1
      val state = queue.dequeue
      for (i <- 0 until 23) {
        val c = state.charAt(i)
        if (c.isLetter && c.isUpper && !inFinalPlace(state, i)) {
          val moves = possibleMoves(state, i)
          // println(s"${queue.size} - $state, $i ($c) : moves are $moves")
          moves.foreach({ case (target, dist) => {
            val nextStateArr = state.toArray
            nextStateArr(target) = c
            nextStateArr(i) = '.'
            val nextState = nextStateArr.mkString
            val nextPrice = states(state) + (price(c) * dist)
            val origPrice = states.get(nextState)
            if (origPrice.isEmpty || origPrice.get > nextPrice) {
              states = states.updated(nextState, nextPrice)
              queue.enqueue(nextState)
              if (interesting.exists(i => state.startsWith(i) || nextState.startsWith(i))) {
                println(s"$state (${states(state)}) -> $nextState ($nextPrice)")
              }
            }
            if (veryInteresting.exists(i => state.startsWith(i) || nextState.startsWith(i))) {
                println(s"$state (${states(state)}) -> $nextState ($nextPrice) [$origPrice] !!!")
              }
          }})
        }
      }
    }

    states.getOrElse("AABBCCDD.......AABBCCDD", -1)
  }

  def main(args: Array[String]): Unit = {
    val initialState = ".D.C.B.A.......D.B.A.C.".toArray

    // Ignore first two lines
    StdIn.readLine()
    StdIn.readLine()

    val firstPattern = """###(\w)#(\w)#(\w)#(\w)###\s*""".r
    val secondPattern = """  #(\w)#(\w)#(\w)#(\w)#\s*""".r

    StdIn.readLine() match {
      case firstPattern(p1, p2, p3, p4) => {
        initialState(0) = p1.charAt(0)
        initialState(2) = p2.charAt(0)
        initialState(4) = p3.charAt(0)
        initialState(6) = p4.charAt(0)
      }
      case str => throw new Exception(s"Unexpected string $str")
    }

    StdIn.readLine() match {
      case secondPattern(p1, p2, p3, p4) => {
        initialState(16) = p1.charAt(0)
        initialState(18) = p2.charAt(0)
        initialState(20) = p3.charAt(0)
        initialState(22) = p4.charAt(0)
      }
      case str => throw new Exception(s"Unexpected string $str")
    }
    // Ignore last line
    StdIn.readLine()

    println(initialState.toList.mkString)
    println(solve1(initialState.mkString))

    // println(possibleMoves(".ABBCC.....DDA.", 12))
  }
}
