import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer

object main {

  def printImage(image: Array[Array[Boolean]]): Unit = {
    image.foreach(line => {
      line.foreach(p => print(if (p) '#' else '.'))
      println()    
    })
  }

  def getNumAt(x: Int, y: Int, defaultPixel: Boolean, image: Array[Array[Boolean]]): Int = {
    def getPixel(y: Int)(x: Int): Boolean = {
      if (y >= 0 && y < image.length) {
        val line = image(y)
        if (x >= 0 && x < line.length) {
          line(x)
        } else {
          defaultPixel
        }
      } else {
        defaultPixel
      }
    }
    List(
      getPixel(y - 1)(x - 1),
      getPixel(y - 1)(x),
      getPixel(y - 1)(x + 1),
      getPixel(y)(x - 1),
      getPixel(y)(x),
      getPixel(y)(x + 1),
      getPixel(y + 1)(x - 1),
      getPixel(y + 1)(x),
      getPixel(y + 1)(x + 1),
    ).foldLeft(0)((res, pixel) => (res * 2) + (if (pixel) 1 else 0))
  }

  def process(image: Array[Array[Boolean]], algorithm: Array[Boolean], defaultPixel: Boolean): Array[Array[Boolean]] = {
    val h = image.length
    val w = image.head.length

    val newImage = Array.ofDim[Boolean](h + 2, w + 2)
    for (y <- 0 until (h + 2)) {
      for (x <- 0 until (w + 2)) {
        newImage(y)(x) = algorithm(getNumAt(x - 1, y - 1, defaultPixel, image))
      }
    }

    newImage
  }

  def processRepeat(image: Array[Array[Boolean]], algorithm: Array[Boolean], left: Int, done: Int = 0): Array[Array[Boolean]] = {
    val defaultPixel = algorithm.head && (done % 2) == 1
    val next = process(image, algorithm, defaultPixel)
    if (left == 0) {
      next
    } else {
      processRepeat(next, algorithm, left - 1, done + 1)
    }
  }

  def main(args: Array[String]): Unit = {
    val algorithm = StdIn.readLine().map(c => c == '#').toArray
    StdIn.readLine() // empty line
    
    val linesBuf = new ArrayBuffer[Array[Boolean]]()
    var lastLine = StdIn.readLine()
    while (lastLine != null) {
      linesBuf.addOne(lastLine.map(c => c == '#').toArray)
      lastLine = StdIn.readLine()
    }
    val image = linesBuf.toArray
    printImage(image)
    println()

    val p1 = process(image, algorithm, false)
    printImage(p1)
    println()

    val p2 = process(p1, algorithm, algorithm.head)
    printImage(p2)
    println()

    println(p2.map(line => line.filter(p => p).length).sum)

    println("Part two")
    val res = processRepeat(image,algorithm, 49)
    println(res.map(line => line.filter(p => p).length).sum)
  }
}
