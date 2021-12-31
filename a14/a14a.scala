import scala.io.StdIn

case class Rule(a: Char, b: Char, insert: Char)

object main {

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList
    val templateLine::_::ruleLines = lines

    val regexp = """(\w)(\w) -> (\w)""".r

    val template = templateLine.toList
    val rules = ruleLines.map({
      case regexp(a, b, between) => Rule(a.charAt(0), b.charAt(0), between.charAt(0))
      case _ => throw new Exception("Bad pattern")
    })

    val ruleMap = rules.foldLeft(Map[(Char, Char), Char]())((map, rule) => map.updated((rule.a, rule.b), rule.insert))

    def process(t: List[Char]): List[Char] = {
      val l = t.length
      val array = Array.ofDim[Char](l + l - 1)
      t.zipWithIndex.foreach({ case (c, index) => array(index * 2) = c })
      for (i <- 1.to(array.length - 2, 2)) {
        array(i) = ruleMap((array(i - 1), array(i + 1)))
      }
      array.toList
    }

    def processTimes(times: Int): List[Char] = {
      var t = template
      for (i <- 0 until times) {
        t = process(t)
      }
      t
    }

    def getValue(t: List[Char]): Int = {
      val counts = t.groupBy(identity).view.mapValues(l => l.size).toMap.toList.sortBy(_._2)
      counts.last._2 - counts.head._2
    }

    val resTemplate = processTimes(40)
    val resValue = getValue(resTemplate)

    println(resTemplate)
    println(resValue)
  }
}
