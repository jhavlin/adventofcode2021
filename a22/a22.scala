import scala.io.StdIn

case class Range(start: Int, end: Int)

case class Instruction(on: Boolean, xRange: Range, yRange: Range, zRange: Range)

object main {

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList
    val regexp = """(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)""".r
    val instructions = lines.map(l => l match {
      case regexp(state, xStart, xEnd, yStart, yEnd, zStart, zEnd) =>
        Instruction(state == "on", Range(xStart.toInt, xEnd.toInt + 1), Range(yStart.toInt, yEnd.toInt + 1), Range(zStart.toInt, zEnd.toInt + 1))
      case _ => throw new Exception()
    })

    val xSpecified = instructions.flatMap({ case Instruction(_, Range(xStart, xEnd), _, _) => List(xStart, xEnd) })
    val ySpecified = instructions.flatMap({ case Instruction(_, _, Range(yStart, yEnd), _) => List(yStart, yEnd) })
    val zSpecified = instructions.flatMap({ case Instruction(_, _, _, Range(zStart, zEnd)) => List(zStart, zEnd) })

    val xValues = (-50 :: 51 :: xSpecified).distinct.sorted.toArray
    val yValues = (-50 :: 51 :: ySpecified).distinct.sorted.toArray
    val zValues = (-50 :: 51 :: zSpecified).distinct.sorted.toArray

    val xMap = xValues.zipWithIndex.toMap
    val yMap = yValues.zipWithIndex.toMap
    val zMap = zValues.zipWithIndex.toMap

    val a = Array.ofDim[Boolean](xValues.length, yValues.length, zValues.length)

    for (i @ Instruction(on, Range(xStart, xEnd), Range(yStart, yEnd), Range(zStart, zEnd)) <- instructions) {
      println(i)
      val xIndexStart = xMap(xStart)
      val xIndexEnd = xMap(xEnd)
      for (x <- xIndexStart until xIndexEnd) {
        val yIndexStart = yMap(yStart)
        val yIndexEnd = yMap(yEnd)
        for (y <- yIndexStart until yIndexEnd) {
          val zIndexStart = zMap(zStart)
          val zIndexEnd = zMap(zEnd)
          for (z <- zIndexStart until zIndexEnd) {
            a(x)(y)(z) = on
          }
        }  
      }
    }
  
    var sum = 0L

    for (x <- 0 until xValues.length) {
      println(s"x $x of ${xValues.length} (${x.toDouble / xValues.length})")
      for (y <- 0 until yValues.length) {
        for (z <- 0 until zValues.length) {
          if (a(x)(y)(z)) {
            val xFrom = xValues(x)
            val xTo = xValues(x + 1) - 1

            val yFrom = yValues(y)
            val yTo = yValues(y + 1) - 1

            val zFrom = zValues(z)
            val zTo = zValues(z + 1) - 1

            if (xFrom >= -50 && xTo <= 50 && yFrom >= -50 && yTo <= 50 && zFrom >= -50 && zTo <= 50) {
              sum += (xTo - xFrom + 1) * (yTo - yFrom + 1) * (zTo - zFrom + 1)
            }           
          }
        }
      }
    }

    println(sum)

    var sum2 = 0L

    for (x <- 0 until xValues.length) {
      println(s"x $x of ${xValues.length} (${x.toDouble / xValues.length})")
      for (y <- 0 until yValues.length) {
        for (z <- 0 until zValues.length) {
          if (a(x)(y)(z)) {
            val xFrom = xValues(x).toLong
            val xTo = xValues(x + 1).toLong - 1

            val yFrom = yValues(y).toLong
            val yTo = yValues(y + 1).toLong - 1

            val zFrom = zValues(z).toLong
            val zTo = zValues(z + 1).toLong - 1

            sum2 += (xTo - xFrom + 1) * (yTo - yFrom + 1) * (zTo - zFrom + 1)
          }
        }
      }
    }

    // real    55507333315707
    // valid 2758514936282235

    println(sum2)
  }
}