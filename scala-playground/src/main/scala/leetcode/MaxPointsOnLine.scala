package leetcode

import scala.math.BigDecimal.RoundingMode.HALF_UP

object MaxPointsOnLine {

  /**
   * Given n points on a 2D plane, find the maximum number of points that lie on the same straight line.
   */
  case class Point(x: BigDecimal, y: BigDecimal)

  val Zero = BigDecimal(0)
  val One = BigDecimal(1)
  val Precision = 20

  def maxPoints(p: Array[Array[Int]]): Int = {
    val points = p.map(a => Point(a(0), a(1))).toList
    points.map { p1 =>
      val lines = points.collect { case p2 if p1.ne(p2) =>
        val a = (p1.y - p2.y).prec
        val b = (p2.x - p1.x).prec
        val c = (p1.x * p2.y - p2.x * p1.y).prec
        if (a == Zero && b == Zero) {
          (Zero, Zero, c)
        } else if (a == Zero) {
          (Zero, One, (c / b).prec)
        } else {
          (One, (b / a).prec, (c / a).prec)
        }
      }
      val groupedPointsPerLine = lines.groupBy { case (a, b, c) => (a, b, c) }
      val equalPoints = points.count(p2 => p1 == p2 && p1.ne(p2))
      val ints = groupedPointsPerLine.map {
        case ((Zero, Zero, Zero), lines) => lines.length
        case (_, lines) => lines.length + equalPoints
      }
      ints.foldLeft(0) { (max, e) => math.max(max, e) } + 1
    }.foldLeft(0) {
      case (max, e) => math.max(max, e)
    }
  }

  implicit class BigDecimalPrec(d: BigDecimal) {
    def prec: BigDecimal = d.setScale(Precision, HALF_UP)
  }

}
