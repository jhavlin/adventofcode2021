import scala.io.StdIn

object main {
  
  val scores  = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val values  = Map(')' -> 1L, ']' -> 2L, '}' -> 3L, '>' -> 4L)
  val starting = Map('}' -> '{', ')' -> '(', ']' -> '[', '>' -> '<')
  val ending = Map('{' -> '}', '(' -> ')', '[' -> ']', '<' -> '>')

  def findFirstInvalidChar(input: List[Char], stack: List[Char]): Option[Char] = {
    input match {
      case c :: rest => {
        ending.get(c) match {
          case Some(endChar) => findFirstInvalidChar(rest, endChar +: stack)
          case None => {
            stack match {
              case stackTop :: stackRest if stackTop == c => findFirstInvalidChar(rest, stackRest)
              case _ => Some(c)
            }
          }
        }
      }
      case Nil => None
    }
  }

  def solve1(lines: List[String]): Long = {
    val points = lines.map(l => findFirstInvalidChar(l.toList, List())).map(charOpt => charOpt.map(c => scores(c)).getOrElse(0))
    points.sum
  }

  def findEndingsForIncomplete(input: List[Char], stack: List[Char]): List[Char] = {
    input match {
      case c :: rest => {
        ending.get(c) match {
          case Some(endChar) => findEndingsForIncomplete(rest, endChar +: stack)
          case None => {
            stack match {
              case stackTop :: stackRest if stackTop == c => findEndingsForIncomplete(rest, stackRest)
              case _ => Nil // Corrupted string, ignore it by returning empty list
            }
          }
        }
      }
      case Nil => stack // Incomplete (stack in non-empty) or valid string (stack is empty)
    }
  }

  def valueOfEnding(ending: List[Char]): Long = {
    ending.foldLeft(0L)((v, c) => v * 5L + values(c))
  }

  def solve2(lines: List[String]): Long = {
    val endings = lines.map(l => findEndingsForIncomplete(l.toList, List())).filter(l => l.nonEmpty)
    val values = endings.map(valueOfEnding(_))
    val sorted = values.sorted
    val middle = sorted.length / 2
    sorted(middle)
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList

    println(solve1(lines))
    println(solve2(lines))
  }
}
