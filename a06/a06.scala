import scala.io.StdIn

object main {

  def nextStep(counts: Map[Int, Long]): Map[Int, Long] = {
    var nextMap = Map[Int, Long]()
    for (v <- 1 to 8) {
      nextMap = nextMap + ((v - 1) -> counts.getOrElse(v, 0))
    }
    nextMap = nextMap + (8 -> counts.getOrElse(0, 0L))
    nextMap = nextMap.updatedWith(6)((orig) => {
      orig match {
        case Some(v) => Some(v + counts.getOrElse(0, 0L))
        case None    => Some(counts.getOrElse(0, 0L))
      }
    })
    nextMap
  }

  def run(counts: Map[Int, Long], days: Int): Map[Int, Long] = {
    var state = counts
    for (d <- 0 until days) {
      state = nextStep(state)
    }
    state
  }

  def total(counts: Map[Int, Long]): Long = {
    counts.values.sum
  }

  def main(args: Array[String]): Unit = {
    val ints = StdIn.readLine().split(",").toList.map(_.toInt);
    val initialCounts =
      ints.groupBy(identity).view.mapValues(_.size.toLong).toMap

    println(total(run(initialCounts, 80)))
    println(total(run(initialCounts, 256)))
  }
}
