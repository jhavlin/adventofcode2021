import scala.io.StdIn

object M {
  val MAPPERS: List[(Int, Int, Int) => (Int, Int, Int)] = List(
    (x: Int, y: Int, z: Int) => (x, y, z),
    (x: Int, y: Int, z: Int) => (x, -z, y),
    (x: Int, y: Int, z: Int) => (x, -y, -z),
    (x: Int, y: Int, z: Int) => (x, z, -y),
    (x: Int, y: Int, z: Int) => (-x, -y, z),
    (x: Int, y: Int, z: Int) => (-x, -z, -y),
    (x: Int, y: Int, z: Int) => (-x, y, -z),
    (x: Int, y: Int, z: Int) => (-x, z, y),
    (x: Int, y: Int, z: Int) => (y, -x, z),
    (x: Int, y: Int, z: Int) => (y, z, x),
    (x: Int, y: Int, z: Int) => (y, -z, -x),
    (x: Int, y: Int, z: Int) => (y, x, -z),
    (x: Int, y: Int, z: Int) => (-y, -z, x),
    (x: Int, y: Int, z: Int) => (-y, -x, -z),
    (x: Int, y: Int, z: Int) => (-y, z, -x),
    (x: Int, y: Int, z: Int) => (-y, x, z),
    (x: Int, y: Int, z: Int) => (z, x, y),
    (x: Int, y: Int, z: Int) => (z, -y, x),
    (x: Int, y: Int, z: Int) => (z, -x, -y),
    (x: Int, y: Int, z: Int) => (z, y, -x),
    (x: Int, y: Int, z: Int) => (-z, -x, y),
    (x: Int, y: Int, z: Int) => (-z, -y, -x),
    (x: Int, y: Int, z: Int) => (-z, x, -y),
    (x: Int, y: Int, z: Int) => (-z, y, x),
  )
}

case class ScannerDef(name: String, coordsList: List[(Int, Int, Int)]) {
  def orientations = M.MAPPERS.map(mapper => Orientation(this, mapper))

  def baseOrientation = Orientation(this, M.MAPPERS.head)

  def baseShift = Shift(baseOrientation, 0, 0, 0)
}

case class Orientation(sDef: ScannerDef, mapper: (Int, Int, Int) => (Int, Int, Int)) {

  def inBaseRotation(coord: (Int, Int, Int)): (Int, Int, Int) = {
    mapper(coord._1, coord._2, coord._3)
  }

  def shiftsToMatch(coord: (Int, Int, Int)): List[Shift] = {
    sDef.coordsList.map(refCoord => {
      val (refX, refY, refZ) = inBaseRotation(refCoord)
      // println(s"shift to match - $coord in base rotation is $refX, $refY, $refZ")
      val (x, y, z) = coord
      Shift(this, x - refX, y - refY, z - refZ)
    })
  }

  def coords = sDef.coordsList.map(c => inBaseRotation(c))
}

case class Shift(orientation: Orientation, sx: Int, sy: Int, sz: Int) {
  def inBaseRotation(coord: (Int, Int, Int)): (Int, Int, Int) = {
    val (x, y, z) = orientation.inBaseRotation(coord)
    (x + sx, y + sy, z + sz)
  }

  def coords = orientation.sDef.coordsList.map(c => inBaseRotation(c))

  def shiftInfo = s"shift $sx, $sy, $sz"
}

object main {

  def areMatching(s1: Shift, s2: Shift): Boolean = {
    var matching = 0
    val cs1 = s1.coords
    val cs2 = s2.coords
    cs1.foreach(c1 => {
      if (cs2.contains(c1)) {
        matching += 1
      }
    })
    // println(s"match count $matching")
    matching >= 6
  }

  def findMatchingShift(ref: Shift, of: ScannerDef): Option[Shift] = {
    for (or <- of.orientations) {
      for (c <- ref.coords) {
        val shifts = or.shiftsToMatch(c)
        val found = shifts.find(shift => areMatching(ref, shift))
        if (found.isDefined) {
          return found;
        }
      }
    }
    None
  }

  def lineToCoords(str: String): (Int, Int, Int) = {
    str.split(",").toList.map(_.toInt) match {
      case a :: b :: c :: _ => (a, b, c)
      case _ => throw new Exception
    }
  }

  def orient(scannerDefs: List[ScannerDef]): List[Shift] = {
    val first :: rest = scannerDefs
    var oriented = List(first.baseShift)
    var missing = rest.toSet

    def orientOne(m: ScannerDef): Unit = {
      for (o <- oriented) {
        val found = findMatchingShift(o, m)
        if (found.isDefined) {
          missing = missing - m
          oriented = found.get +: oriented
          return
        }
      }
    }

    while (missing.nonEmpty) {
      println(s"${missing.size}")
      for (m <- missing) {
        orientOne(m)
      }
    }
    oriented.reverse
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList

    val scannerDefs: List[ScannerDef] = lines.foldLeft(List[ScannerDef]())((scanners, line) => {
      if (line.trim.isEmpty) {
        scanners
      } else if (line.trim.startsWith("---")) {
        ScannerDef(line.trim, List()) +: scanners
      } else {
        scanners match {
          case (ScannerDef(name, coordList) :: rest) => ScannerDef(name, lineToCoords(line) +: coordList) +: rest
          case _ => throw new Exception
        }
      }
    }).map({ case ScannerDef(name, list) => ScannerDef(name, list.reverse)}).reverse

    val oriented = orient(scannerDefs)
    println(oriented)

    val all = oriented.flatMap(shift => shift.coords).distinct
    println(all)
    println(all.size)

    val pairs = for (a <- oriented; b <- oriented) yield (a, b)
    var dists = pairs.map({ case (a, b) => Math.abs(a.sx - b.sx) + Math.abs(a.sy - b.sy) + Math.abs(a.sz - b.sz)})
    println(dists.max)
  }
}
