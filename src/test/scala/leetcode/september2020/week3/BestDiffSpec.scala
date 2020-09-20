package leetcode.september2020.week3

import leetcode.september2020.week3.SequentialDigits.sequentialDigits
import org.scalatest.flatspec._
import org.scalatest.matchers._

class BestDiffSpec extends AnyFlatSpec with should.Matchers {
  "BestDiff" should "find Best Time to Buy and Sell Stock" in {
    BestDiff.maxProfit(Array(7,1,5,3,6,4)) should be(5)
    BestDiff.maxProfit(Array(7,6,4,3,1)) should be(0)
    BestDiff.maxProfit(Array(7,2,4,1)) should be(2)
    BestDiff.maxProfit(Array(7,4,1,2)) should be(1)
  }

}
