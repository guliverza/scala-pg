package leetcode.september2020.week2

import leetcode.september2020.week2.CombinationSumIII.combinationSum3
import org.scalatest.flatspec._
import org.scalatest.matchers._

class CombinationSumIIISpec extends AnyFlatSpec with should.Matchers {
  "Bulls and Cows" should "provide hints" in {
    combinationSum3(1, 9) shouldBe List(List(9))
    combinationSum3(3, 9) shouldBe List(List(1, 2, 6), List(1, 3, 5), List(2, 3, 4))
    combinationSum3(3, 7) shouldBe List(List(1, 2, 4))
    combinationSum3(2, 18) shouldBe Nil
    combinationSum3(2, 6) shouldBe List(List(1, 5), List(2, 4))
    combinationSum3(9, 4) shouldBe Nil
  }
}
