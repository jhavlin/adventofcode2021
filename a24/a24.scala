import scala.io.StdIn

object main {

  // Not used
  type Vars = Map[Char, Long]
  val lineRegexp = """(\w+) (\w) (\S+)""".r

  // Not used
  def segments(lines: List[String]): List[List[String]] = {
    val reversed = lines.foldLeft(List[List[String]]())((acc, current) => {
      if (current.startsWith("inp")) {
        List(current) :: acc
      } else {
        acc match {
          case list :: rest => (current :: list) :: rest
          case _ => throw new Error()
        }
      }
    })
    reversed.map(l => l.reverse).reverse
  }

  // Not used
  def processLine(line: String, vars: Vars): Vars = {
    line match {
      case lineRegexp(instr, a1, a2) => {
        val v1 = vars.getOrElse(a1.charAt(0), 0L)
        val v2 = if (a2.charAt(0).isLetter) vars.getOrElse(a2.charAt(0), 0L) else a2.toLong
        vars.updated(a1.charAt(0), instr match {
          case "add" => { v1 + v2 }
          case "mul" => { v1 * v2 }
          case "div" => { v1 / v2 }
          case "mod" => { v1 % v2 }
          case "eql" => { if (v1 == v2) 1L else 0L }
          case _ => throw new Error()
        })
      }
      case _ => throw new Error()
    }
  }

  // Not used
  def processSequence(lines: List[String], vars: Vars): Vars = {
    var state = vars
    lines.foreach(line => {
      // println(line)
      state = processLine(line, state)
      //println(state)
    })
    state
  }

  // Not used
  def processSegment(lines: List[String], statesToBestInputMap: Map[Vars, Long]): Map[Vars, Long] = {
    var res = Map[Vars, Long]()
    for (origState <- statesToBestInputMap) {
      for (input <- 1 to 9) {
        val stateAfterInput = origState._1.updated('w', input.toLong)
        val stateAfterSequence = processSequence(lines.tail, stateAfterInput)
        val fullInput = (origState._2 * 10) + input
        res = res.updatedWith(stateAfterSequence) {
          case Some(fi) if fullInput > fi => Some(fullInput) 
          case Some(fi) => Some(fi)
          case None => Some(fullInput)
        }
      }
    }
    res
  }

  // Not used
  def run(inputs: List[Long], instructionSegments: List[List[String]]): Vars = {
    var vars = Map('w' -> 0L, 'x' -> 0L, 'y' -> 0L, 'z' -> 0L)
    inputs.zip(instructionSegments).foreach({
      case (input, lines) => {
        lines match {
          case "inp w" :: rest => { vars = processSequence(rest, vars.updated('w', input)) }
          case "inp x" :: rest => { vars = processSequence(rest, vars.updated('x', input)) }
          case "inp y" :: rest => { vars = processSequence(rest, vars.updated('y', input)) }
          case "inp z" :: rest => { vars = processSequence(rest, vars.updated('z', input)) }
          case _ => throw new Error()
        }
      }
    })
    vars
  }

  def segmentToImportant(segment: List[String]): (Int, Int, Int) = {
    val arr = segment.toArray
    val l5 = arr(4)
    val l6 = arr(5)
    val l16 = arr(15)
    val d = l5.split(" ")(2).toInt
    val a = l6.split(" ")(2).toInt
    val b = l16.split(" ")(2).toInt
    (d, a, b)
  }

  def segmentsToImportant(segments: List[List[String]]): List[(Int, Int, Int)] = {
    segments.map(segmentToImportant)
  }

  def solve(important: List[(Int, Int, Int)]): List[Long] = {
    def s(remaining: List[(Int, Int, Int)], inputs: List[Int], stack: List[Int]): List[List[Int]] = {

      if (remaining.isEmpty) {
        if (stack.isEmpty) {
          List(inputs)
        } else {
          List()
        }
      } else {
        val (d, a, b)::rest = remaining
        if (d == 1) {
          (1 to 9).toList.flatMap(input => s(rest, input :: inputs, (input + b) :: stack))
        } else { // d == 26
          val top::stackRest = stack
          val input = top + a
          if (input >= 1 && input <= 9) {
            s(rest, input :: inputs, stackRest)
          } else {
            List()
          }
        }
      }
    }
    val solutions = s(important, List[Int](), List[Int]())
    solutions.map(s => s.reverse.mkString.toLong)
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList.filter(l => l.nonEmpty && !l.startsWith("#"))
    val instructionSegments = segments(lines)

    val important = segmentsToImportant(instructionSegments);
    // println(important)

    val solutions = solve(important)

    println(solutions)
    println(s"max ${solutions.max}")
    println(s"min ${solutions.min}")
  }
}
