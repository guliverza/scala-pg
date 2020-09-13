package leetcode.september2020.week2

import leetcode.september2020.common.Interval

object Intervals {

  /**
   * Given a set of non-overlapping intervals, insert a new interval into the intervals (merge if necessary).
   * You may assume that the intervals were initially sorted according to their start times.
   */
  def insert(_intervals: Array[Array[Int]], _newInterval: Array[Int]): Array[Array[Int]] = {
    val intervals = _intervals.map(Interval(_)).toList
    val newInterval = Interval(_newInterval)

    val (before, after) = intervals.span(_.before(newInterval))
    val (in, tail) = after.span(_.intersects(newInterval))
    val middle = Interval(
      from = math.min(newInterval.from, in.headOption.map(_.from).getOrElse(newInterval.from)),
      to = math.max(newInterval.to, in.lastOption.map(_.to).getOrElse(newInterval.to)))
    ((before :+ middle) ++ tail).map(_.asArray).toArray
  }
}
