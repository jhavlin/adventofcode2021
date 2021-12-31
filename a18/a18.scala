import scala.io.StdIn

sealed abstract class Num {

  type Path = List[Char]

  def niceStr: String

  def add(that: Num): Num = Pair(this, that).reduced

  def replaceLastValueInPath(value: Char, newValue: Char, path: Path): Path = {
    def inner(p: Path): (Boolean, Path) = {
      p match {
        case Nil => (false, Nil)
        case v :: rest => {
          val (alreadyReplaced, res) = inner(rest)
          if (v == value && !alreadyReplaced) {
            (true, newValue +: Nil)
          } else {
            (alreadyReplaced, v +: res)
          }
        }
      }
    }
    inner(path)._2
  }

  def increment(num: Num, path: Path, defaultDirection: Char, incrementValue: Long): Num = {
    num match {
      case Pair(left, right) => {
        val d = path.headOption.getOrElse(defaultDirection)
        if (d == 'L') {
          Pair(increment(left, path.drop(1), defaultDirection, incrementValue), right)
        } else { // d == 'P'
          Pair(left, increment(right, path.drop(1), defaultDirection, incrementValue))
        }
      }
      case Regular(value) => Regular(value + incrementValue)
    }
  }

  def explodedIfNeeded: Option[Num] = {

    def replacePair(num: Num, level: Int, path: Path): (Num, Option[(Path, Pair)]) = {
      num match {
        case p @ Pair(Regular(leftValue), Regular(rightValue)) if level >= 4 => (Regular(0), Some((path, p)))
        case Pair(left, right) => {
          val leftResult = replacePair(left, level + 1, 'L' +: path)
          val (leftTree, pathAndOrigOpt) = leftResult
          pathAndOrigOpt match {
            case Some(r) => (Pair(leftTree, right), pathAndOrigOpt)
            case None => {
              val rightResult = replacePair(right, level + 1, 'P' +: path)
              val (rightTree, pathAndOrigOptRight) = rightResult
              (Pair(left, rightTree), pathAndOrigOptRight)
            }
          }
        }
        case Regular(_) => (num, None)
      }
    }

    val replaced = replacePair(this, 0, List())

    replaced match {
      case (tree, None) => None
      case (tree, Some((path, origPair))) => {
        val (incrementLeft, incrementRight) = origPair match {
          case Pair(Regular(l), Regular(r)) => (l, r)
          case _ => (0L, 0L)
        }
        // println(s"path $path")
        val fixedLeft = if (path.contains('P')) {
          val leftPath = replaceLastValueInPath('P', 'L', path.reverse)
          // println(s"left path $leftPath")
          increment(tree, leftPath, 'P', incrementLeft)
        } else {
          tree
        }
        
        val fixedRight = if (path.contains('L')) {
          val rightPath = replaceLastValueInPath('L', 'P', path.reverse)
          // println(s"right path $rightPath")
          increment(fixedLeft, rightPath, 'L', incrementRight)
        } else {
          fixedLeft
        }

        Some(fixedRight)
      }
    }
  }

  def splitIfNeeded: Option[Num] = {
    def split(num: Num): (Boolean, Num) = {
      num match {
        case Pair(left, right) => {
          val (leftSplit, leftTree) = split(left)
          if (leftSplit) {
            (leftSplit, Pair(leftTree, right))
          } else {
            val (rightSplit, rightTree) = split(right)
            (rightSplit, Pair(left, rightTree))
          }
        }
        case Regular(v) if v >= 10 => (true, Pair(Regular(v / 2), Regular((v + 1) / 2)))
        case n => (false, n) 
      }
    }
    val (changed, tree) = split(this)
    if (changed) {
      Some(tree)
    } else {
      None
    }    
  }

  def reduced:Num = {
    var n = this
    var continue = true
    while (continue) {
      n.explodedIfNeeded match {
        case Some(updated) => { n = updated }
        case None => {
          n.splitIfNeeded match {
            case Some(updated2) => { n = updated2 }
            case None => { continue = false }
          }
        }
      }
    }
    n
  }

  def magnitude: Long
}
case class Regular(value: Long) extends Num {
  def niceStr = value.toString

  def magnitude = value
}
case class Pair(left: Num, right: Num) extends Num {
  def niceStr = s"[${left.niceStr},${right.niceStr}]"

  def magnitude = 3 * left.magnitude + 2 * right.magnitude
}

object main {

  def parse(str: String, pos: Int): (Int, Num) = {
    if (str.charAt(pos) == '[') {
      val (endLeft, left) = parse(str, pos + 1)
      val (endRight, right) = parse(str, endLeft + 1)
      (endRight + 1, Pair(left, right))
    } else {
      var i = pos
      var value = 0L
      while (str.charAt(i).isDigit) {
        value = value * 10 + str.charAt(i).asDigit
        i += 1
      }
      (i, Regular(value))
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList
    val numbers = lines.map(parse(_, 0)._2)
    val res = numbers.reduce((a, b) => a.add(b))
    println(res.niceStr)
    println(res.magnitude)

    val pairs = for (x <- numbers; y <- numbers if x != y) yield (x, y)
    val res2 = pairs.map({ case (a, b) => a.add(b).magnitude }).max
    println(res2)
  }
}
