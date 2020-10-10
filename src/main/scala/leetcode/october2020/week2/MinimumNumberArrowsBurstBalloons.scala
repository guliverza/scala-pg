package leetcode.october2020.week2

import common.Interval

/**There are some spherical balloons spread in two-dimensional space.
 * For each balloon, provided input is the start and end coordinates of the horizontal diameter.
 * Since it's horizontal, y-coordinates don't matter,
 * and hence the x-coordinates of start and end of the diameter suffice.
 * The start is always smaller than the end.
 *
 * An arrow can be shot up exactly vertically from different points along the x-axis.
 * A balloon with xstart and xend bursts by an arrow shot at x if xstart ≤ x ≤ xend.
 * There is no limit to the number of arrows that can be shot.
 * An arrow once shot keeps traveling up infinitely.
 *
 * Given an array points where points[i] = [xstart, xend], return the minimum number of arrows
 * that must be shot to burst all balloons.
 */
object MinimumNumberArrowsBurstBalloons {

  def findMinArrowShots(points: Array[Array[Int]]): Int = {
    val intervals = points.toList.map(Interval.apply).sortBy(_.from)
    val unique = intervals.foldLeft(List.empty[Interval]) {
      case (Nil, p) => List(p)
      case (current :: tail, interval) if current.intersects(interval) => current.intersection(interval) :: tail
      case (current :: tail, interval) => interval :: current :: tail
    }.reverse
    unique.size
  }

  def main(args: Array[String]): Unit = {
    println(Interval(1,2).intersects(Interval(2,3)))
    println(findMinArrowShots(Array(Array(1,2),Array(3,4),Array(5,6),Array(7,8))))
    println(findMinArrowShots(Array(Array(1,2),Array(2,3),Array(3,4),Array(4,5))))
    println(findMinArrowShots(Array(Array(1,2))))
    println(findMinArrowShots(Array(Array(2,3), Array(2,3))))
    println(findMinArrowShots(Array(Array(1,2),Array(4,5),Array(1,5))))
  }

}
