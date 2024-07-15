package codewars

import common.Interval

object SumOfIntervals {
  def sumOfIntervals(intervals: List[(Int, Int)]): Int = {
    val ints = intervals.map(a => Interval(a)).sortBy(i => i.from)
    val (ints2, c) = ints.tail.foldLeft((List.empty[Interval], ints.head)) {
      case ((res, cur), i) if cur.intersects(i) => (res, cur.union(i))
      case ((res, cur), i) => (res :+ cur, i)
    }
    val noIntersections = ints2 :+ c
    noIntersections.map(i => i.length).sum
  }

}
