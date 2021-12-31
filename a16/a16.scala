import scala.io.StdIn
import scala.math.BigInt

object main {

  val toHex = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111",
  )

  sealed abstract class Packet (version: Int, typ: Int)
  case class Literal(version: Int, typ: Int, value: BigInt) extends Packet(version, typ)
  case class Operator(version: Int, typ: Int, lengthTypeId: Int, operands: List[Packet]) extends Packet(version, typ)

  def parseLiteral(str: String, startPos: Int, version: Int, typ: Int): (Int, Packet) = {
    var curr = startPos
    var continue = true
    var value = ""
    while (continue) {
      val seg = str.substring(curr, curr + 5)
      // println(s"$curr $seg")
      curr += 5
      continue = seg.charAt(0) == '1'
      value = value + seg.tail
    }
    (curr, Literal(version, typ, BigInt(value, 2)))
  }

  def parseOperandL0(str: String, startPos: Int, version: Int, typ: Int): (Int, Packet) = {
    val totalLen = Integer.parseInt(str.substring(startPos, startPos + 15), 2)
    var end = startPos + 15
    var operands = List[Packet]()
    while (end < totalLen + startPos + 15) {
      val (e, p) = parse(str, end)
      operands = p +: operands
      end = e
    }
    (startPos + 15 + totalLen, Operator(version, typ, 0, operands.reverse))
  }

  def parseOperandL1(str: String, startPos: Int, version: Int, typ: Int): (Int, Packet) = {
    val count = Integer.parseInt(str.substring(startPos, startPos + 11), 2)
    var end = startPos + 11
    var operands = List[Packet]()
    var i = 0
    while (i < count) {
      val (e, p) = parse(str, end)
      operands = p +: operands
      end = e
      i += 1
    }
    (end, Operator(version, typ, 1, operands.reverse))
  }

  def parseOperand(str: String, startPos: Int, version: Int, typ: Int): (Int, Packet) = {
    val lengthTypeId = str.charAt(startPos).asDigit
    if (lengthTypeId == 0) {
      parseOperandL0(str, startPos + 1, version, typ)
    } else {
      parseOperandL1(str, startPos + 1, version, typ)
    }
  }

  def parse(str: String, startPos: Int): (Int, Packet) = {
    val versionStr = str.substring(startPos, startPos + 3)
    val typeStr = str.substring(startPos + 3, startPos + 6)
    val version = Integer.parseInt(versionStr, 2)
    val typ = Integer.parseInt(typeStr, 2)
    // println(s"version $version typ $typ versionStr $versionStr typeStr $typeStr")
    if (typ == 4) {
      parseLiteral(str, startPos + 6, version, typ)
    } else {
      parseOperand(str, startPos + 6, version, typ)
    }
  }

  def sumVersions(p: Packet): Int = {
    p match {
      case Literal(v, _, _) => v
      case Operator(v, _, _, operands) => v + operands.map(o => sumVersions(o)).sum
    }
  }

  def eval(p: Packet): Long = {
    p match {
      case Literal(_, _, value) => value.toLong
      case Operator(_, typ, _, operands) if typ == 0 => operands.map(o => eval(o)).sum
      case Operator(_, typ, _, operands) if typ == 1 => operands.map(o => eval(o)).product
      case Operator(_, typ, _, operands) if typ == 2 => operands.map(o => eval(o)).min
      case Operator(_, typ, _, operands) if typ == 3 => operands.map(o => eval(o)).max
      case Operator(_, typ, _, operands) if typ == 5 => operands.map(o => eval(o)) match { case a :: b :: _ => if (a > b) 1 else 0 }
      case Operator(_, typ, _, operands) if typ == 6 => operands.map(o => eval(o)) match { case a :: b :: _ => if (a < b) 1 else 0 }
      case Operator(_, typ, _, operands) if typ == 7 => operands.map(o => eval(o)) match { case a :: b :: _ => if (a == b) 1 else 0 }
    }
  }

  def main(args: Array[String]): Unit = {
    val input = StdIn.readLine()
    val binary = input.flatMap(c => toHex(c))
    println(binary)
    val (_, parsed) = parse(binary, 0)
    println(parsed)
    println(sumVersions(parsed))
    println(eval(parsed))
  }
}
