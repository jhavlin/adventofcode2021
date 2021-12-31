import scala.io.StdIn

object main {

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList
    val templateLine::_::ruleLines = lines

    val regexp = """(\w\w) -> (\w)""".r

    val template = "^" + templateLine + "$"
    val rules = ruleLines.map({
      case regexp(pair, between) => (pair, between.charAt(0))
      case _ => throw new Exception("Bad pattern")
    })

    val initialCountsMap = template.sliding(2).toList.groupBy(identity).view.mapValues(_.length.toLong).toMap
    val ruleMap = rules.foldLeft(Map[String, Char]())((map, origInsertPair) => map.updated(origInsertPair._1, origInsertPair._2))

    def process(countsMap: Map[String, Long]): Map[String, Long] = {
      var newMap = Map[String, Long]()
      countsMap.foreach({ case (string, count) => {
        if (string.contains("$") || string.contains("^")) {
          newMap = newMap.updated(string, count)
        } else {
          val insert = ruleMap(string)
          val left = string.charAt(0).toString + insert.toString
          val right = insert.toString + string.charAt(1).toString
          val leftCount = newMap.getOrElse(left, 0L)
          var rightCount = newMap.getOrElse(right, 0L)
          newMap = newMap.updated(left, leftCount + count)
          newMap = newMap.updated(right, rightCount + count)   
        }
      }})
      newMap
    }

    def processTimes(times: Int): Map[String, Long] = {
      var t = initialCountsMap
      for (i <- 0 until times) {
        t = process(t)
      }
      t
    }

    def getValue(countsMap: Map[String, Long]): Long = {
      val charCounts = Array.ofDim[Long](26)
      def add(char: Char, count: Long): Unit = {
        if (char.isLetter) {
          charCounts(char - 'A') += count
        }
      }
      countsMap.foreach({ case (string, count) => {
        add(string.charAt(0), count)
        add(string.charAt(1), count)
      }})
      val validCounts = charCounts.filter(_ > 0)
      validCounts.max / 2 - validCounts.min / 2
    }

    val resCounts = processTimes(40)
    val resValue = getValue(resCounts)

    println(resCounts)
    println(resValue)
  }
}
