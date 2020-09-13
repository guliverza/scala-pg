package leetcode.september2020.week2

import org.scalatest.flatspec._
import org.scalatest.matchers._

class IntervalsSpec extends AnyFlatSpec with should.Matchers {
  "Intervals" should "manager intervals" in {
    Intervals.insert(Array(Array(1, 2), Array(5, 6)), Array(3, 4)) shouldBe Array(Array(1, 2), Array(3, 4), Array(5, 6))
    Intervals.insert(Array(Array(1, 2), Array(3, 4)), Array(5, 6)) shouldBe Array(Array(1, 2), Array(3, 4), Array(5, 6))
    Intervals.insert(Array(Array(3, 4), Array(5, 6)), Array(1, 2)) shouldBe Array(Array(1, 2), Array(3, 4), Array(5, 6))

    Intervals.insert(Array(Array(10, 20), Array(50, 60)), Array(55, 80)) shouldBe Array(Array(10, 20), Array(50, 80))
    Intervals.insert(Array(Array(10, 20), Array(50, 60)), Array(55, 56)) shouldBe Array(Array(10, 20), Array(50, 60))
    Intervals.insert(Array(Array(10, 20), Array(50, 60)), Array(15, 55)) shouldBe Array(Array(10, 60))
  }
}
