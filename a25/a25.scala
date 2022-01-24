import scala.io.StdIn

object main {

  type State = Array[Array[Char]]
  type Flags = Array[Array[Boolean]]

  def clone(state: State): State = {
    state.map(_.toList).toList.map(_.toArray).toArray
  }

  def step(state: State): (State, Boolean) = {
    val h = state.length
    val w = state(0).length

    val next = clone(state)
    var changed = false

    var flags = Array.ofDim[Boolean](h, w)
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        val c = next(y)(x)
        if (c == '>') {
          flags(y)(x) = next(y)((x + 1) % w) == '.'
        }
      }
    }
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        if (flags(y)(x)) {
          next(y)(x) = '.'
          next(y)((x + 1) % w) = '>'
          changed = true
        }
      }
    }

    flags = Array.ofDim[Boolean](h, w)
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        val c = next(y)(x)
        if (c == 'v') {
          flags(y)(x) = next((y + 1) % h)(x) == '.'
        }
      }
    }
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        if (flags(y)(x)) {
          next(y)(x) = '.'
          next((y + 1) % h)(x) = 'v'
          changed = true
        }
      }
    }

    (next, changed)
  }

  def printState(state: State): Unit = {
    for (line <- state) {
      for (char <- line) {
        print(char)
      }
      println
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList
    var initial = lines.map(_.toArray).toArray

    // printState(initial)

    var changed = true
    var state = initial
    var i = 0

    while (changed) {
      val res = step(state)
      state = res._1
      changed = res._2
      i = i + 1
    }
    println(i)
  }
}
