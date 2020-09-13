package leetcode.september2020.common

import org.scalatest.flatspec._
import org.scalatest.matchers._

class IntervalSpec extends AnyFlatSpec with should.Matchers {
  "Interval" should "provide some intervals logicc" in {
    Interval(1, 5).intersects(Interval(2, 3)) should be (true)
    Interval(1, 5).intersects(Interval(-1, 1)) should be (true)
    Interval(1, 5).intersects(Interval(5, 6)) should be (true)
    Interval(1, 5).intersects(Interval(-1, 0)) should be (false)
    Interval(1, 5).after(Interval(6, 7)) should be (false)
    Interval(1, 5).before(Interval(-1, 0)) should be (false)
    Interval(1, 5).before(Interval(6, 7)) should be (true)
  }
}
