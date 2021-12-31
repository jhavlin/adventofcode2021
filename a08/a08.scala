import scala.io.StdIn

object main {

  val SEGMENTS_T0_VALUE = Map(
    "abcefg" -> 0,
    "cf" -> 1,
    "acdeg" -> 2,
    "acdfg" -> 3,
    "bcdf" -> 4,
    "abdfg" -> 5,
    "abdefg" -> 6,
    "acf" -> 7,
    "abcdefg" -> 8,
    "abcdfg" -> 9
  )

  val PERMUTATIONS = "abcdefg".permutations.toList.reverse

  def translateChar(c: Char, permutation: String): Char = {
    val pos = c - 'a'
    permutation.charAt(pos)
  }

  def translate(input: String, permutation: String): String = {
    input.map(translateChar(_, permutation)).sorted
  }

  def findPermutation(patterns: List[String]): String = {
    PERMUTATIONS
      .find((permutation) => {
        patterns.forall((pattern) => {
          SEGMENTS_T0_VALUE.get(translate(pattern, permutation)).isDefined
        })
      })
      .get
  }

  def toValue(input: String, permutation: String): Int = {
    SEGMENTS_T0_VALUE(translate(input, permutation))
  }

  def processLine(line: String): String = {
    val parts = line.split("\\|").map(_.trim)
    val patterns = parts(0).split("\\s+").map(_.sorted).toList
    val input = parts(1).split("\\s+").map(_.sorted).toList

    val permutation = findPermutation(patterns)
    input.map(toValue(_, permutation)).map(_.toString).mkString("")
  }

  def main(args: Array[String]): Unit = {
    val values = io.Source.stdin.getLines().map(processLine(_)).toList

    println(
      values
        .mkString("")
        .filter(c => c == '1' || c == '4' || c == '7' || c == '8')
        .size
    )
    println(values.map(_.toInt).sum)
  }
}
