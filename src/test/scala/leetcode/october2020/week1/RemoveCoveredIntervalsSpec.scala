package leetcode.october2020.week1

import org.scalatest.flatspec._
import org.scalatest.matchers._

class RemoveCoveredIntervalsSpec  extends AnyFlatSpec with should.Matchers {
  "RemoveCoveredIntervals" should "filter out covered intervals" in {
    RemoveCoveredIntervals.removeCoveredIntervals(Array(Array(1,4),Array(3,6),Array(2,8))) should be (2)
    RemoveCoveredIntervals.removeCoveredIntervals(Array(Array(1,4),Array(2,3))) should be (1)
    RemoveCoveredIntervals.removeCoveredIntervals(Array(Array(0,10),Array(5,12))) should be (2)
    RemoveCoveredIntervals.removeCoveredIntervals(Array(Array(3,10),Array(4,10),Array(5,11))) should be (2)
    RemoveCoveredIntervals.removeCoveredIntervals(Array(Array(1,2),Array(1,4),Array(3,4))) should be (1)
  }

}
