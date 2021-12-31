import scala.io.StdIn

object main {

  def solve1(s1: Int, s2: Int): Long = {
    var p1 = s1
    var p2 = s2
    var v1 = 0L
    var v2 = 0L
    var rolls = 0L

    var dice = 1

    def getDice(): Int = {
      val d = dice
      dice = dice + 1
      if (dice > 100) {
        dice = 1
      }
      rolls += 1
      d
    }

    while (true) {
      p1 = (p1 + getDice() + getDice() + getDice()) % 10
      v1 = v1 + (p1 + 1)
      if (v1 >= 1000) {
        return v2 * rolls
      }

      p2 = (p2 + getDice() + getDice() + getDice()) % 10
      v2 = v2 + (p2 + 1)
      if (v2 >= 1000) {
        return v1 * rolls
      }
    }
    return -1
  }

  def solve2(s1: Int, s2: Int): (BigInt) = {
    val distanceCombinations = Map[Int, Long](3 -> 1, 4 -> 3, 5 -> 6, 6 -> 7, 7 -> 6, 8 -> 3, 9 -> 1)

    val positions = 10
    val points = 32
    val limit = 21

    val array = Array.ofDim[BigInt](points, points, positions, positions)

    for (pointsA <- 0 until points) {
      for (pointsB <- 0 until points) {
        for (positionA <- 0 until positions) {
          for (positionB <- 0 until positions) {
            array(pointsA)(pointsB)(positionA)(positionB) = BigInt(0)
          }
        }
      }
    }

    array(0)(0)(s1)(s2) = BigInt(1)

    for (pointsA <- 0 until limit) {
      for (pointsB <- 0 until limit) {
        for (positionA <- 0 until positions) {
          for (positionB <- 0 until positions) {
            distanceCombinations.foreach({ case (distanceA, combinationsA) => {

              val nextPositionA = (positionA + distanceA) % positions
              val nextPointsValueA  = pointsA + nextPositionA + 1

              if (nextPointsValueA >= 21) {
                  // no second player
                  array(nextPointsValueA)(pointsB)(nextPositionA)(positionB) += array(pointsA)(pointsB)(positionA)(positionB) * combinationsA
              } else {
                distanceCombinations.foreach({ case (distanceB, combinationsB) => {
                  
                  val nextPositionB = (positionB + distanceB) % positions
                  val nextPointsValueB  = pointsB + nextPositionB + 1                                 
                  
                  array(nextPointsValueA)(nextPointsValueB)(nextPositionA)(nextPositionB) += array(pointsA)(pointsB)(positionA)(positionB) * combinationsA * combinationsB
                }})
              }
            }})
          }
        }
      }
    }
    var res1 = BigInt(0)
    var res2 = BigInt(0)
    for (pointsValueA <- 0 until points) {
      for (pointsValueB <- 0 until points) {
        for (positionValueA <- 0 until positions) {
          for (positionValueB <- 0 until positions) {
            if (pointsValueA >= 21 && pointsValueB < 21) {
              res1 += array(pointsValueA)(pointsValueB)(positionValueA)(positionValueB)
            } else if (pointsValueB >= 21 && pointsValueA < 21) {
              res2 += array(pointsValueA)(pointsValueB)(positionValueA)(positionValueB)
            }
          }
        }
      }
    }

    if (res1 > res2) res1 else res2
  }

  def main(args: Array[String]): Unit = {
    val regexp = """Player (\d+) starting position: (\d+)""".r
    var p1: Int = StdIn.readLine() match {
      case regexp(_, v) => v.toInt - 1
      case _ => throw new Exception()
    }
    var p2: Int = StdIn.readLine() match {
      case regexp(_, v) => v.toInt - 1
      case _ => throw new Exception()
    }

    println(solve1(p1, p2))
    println(solve2(p1, p2))
  }
}
