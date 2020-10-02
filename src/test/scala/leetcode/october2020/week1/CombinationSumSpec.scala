package leetcode.october2020.week1

import org.scalatest.flatspec._
import org.scalatest.matchers._

class CombinationSumSpec extends AnyFlatSpec with should.Matchers {

  "CombinationSum" should "list all combinations" in {
    CombinationSum.combinationSum(Array(2,3,6,7), 7) should be (List(List(2,2,3),List(7)))
    CombinationSum.combinationSum(Array(2), 1) should be (Nil)
    CombinationSum.combinationSum(Array(1), 1) should be (List(List(1)))
    CombinationSum.combinationSum(Array(1), 2) should be (List(List(1, 1)))
  }

}
