package leetcode.october2020.week1

import common.Interval

object RemoveCoveredIntervals {

  def removeCoveredIntervals(_intervals: Array[Array[Int]]): Int = {
    val intervals = _intervals.map(Interval.apply)
    intervals.filterNot(interval => {
      intervals.exists(o => interval != o && interval.coveredBy(o))
    }).length
  }
}
