import scala.io.StdIn

object main {

  def exec(x1: Int, x2: Int, y1: Int, y2: Int, velX: Int, velY: Int): Option[Int] = {
    var x = 0
    var y = 0
    var vx = velX
    var vy = velY
    var maxY = 0
    while (y >= y1) {
      if (x >= x1 && x <= x2 && y >= y1 && y <= y2) {
        return Some(maxY)
      }
      x = x + vx
      y = y + vy
      vx = vx - Math.signum(vx).toInt
      vy = vy - 1
      maxY = Math.max(maxY, y)
    }
    None
  }

  def solve1(x1: Int, x2: Int, y1: Int, y2: Int): (Int, Int) = {
    var res: Option[Int] = None
    var count = 0
    for (velX <- -1000 to 1000) {
      for (velY <- -1000 to 1000) {
        val r: Option[Int] = exec(x1, x2, y1, y2, velX, velY)
        // if (r.isDefined) {
        //  println(s"velX = $velX, velY = $velY, r = $r")
        //}
        res = (res, r) match {
          case (None, None) => None
          case (Some(_), None) => res
          case (None, Some(_)) => r
          case (Some(a), Some(b)) => Some(Math.max(a, b))
        }
        if (r.isDefined) {
          count += 1
        }
      }
    }
    (res.getOrElse(-1), count)
  }

  def main(args: Array[String]): Unit = {
    val input = StdIn.readLine()
    var regexp = """^target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)$""".r
    val (x1, x2, y1, y2) = input match {
      case regexp(s1, s2, s3, s4) => (s1.toInt, s2.toInt, s3.toInt, s4.toInt)
      case _ => throw new Exception
    }
    println(x1, x2, y1, y2)
    println(solve1(x1, x2, y1, y2))
  }
}
