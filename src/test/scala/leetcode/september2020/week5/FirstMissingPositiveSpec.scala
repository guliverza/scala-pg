package leetcode.september2020.week5

import org.scalatest.flatspec._
import org.scalatest.matchers._

class FirstMissingPositiveSpec extends AnyFlatSpec with should.Matchers {
  "FirstMissingPositive" should "find first missing positive integer using BitSet" in {
    FirstMissingPositive.firstMissingPositiveBitSet(Array(1,2,0)) should be (3)
    FirstMissingPositive.firstMissingPositiveBitSet(Array(3,4,-1,1)) should be (2)
    FirstMissingPositive.firstMissingPositiveBitSet(Array(7,8,9,11,12)) should be (1)
    FirstMissingPositive.firstMissingPositiveBitSet(Array()) should be (1)
    FirstMissingPositive.firstMissingPositiveBitSet(Array(1, 1)) should be (2)
  }

  "FirstMissingPositive" should "find first missing positive integer using Array" in {
    FirstMissingPositive.firstMissingPositiveArray(Array(1,2,0)) should be (3)
    FirstMissingPositive.firstMissingPositiveArray(Array(3,4,-1,1)) should be (2)
    FirstMissingPositive.firstMissingPositiveArray(Array(7,8,9,11,12)) should be (1)
    FirstMissingPositive.firstMissingPositiveArray(Array()) should be (1)
    FirstMissingPositive.firstMissingPositiveArray(Array(1)) should be (2)
    FirstMissingPositive.firstMissingPositiveArray(Array(1, 1)) should be (2)
  }

  "FirstMissingPositive" should "find first missing positive integer using constant memory" in {
    FirstMissingPositive.firstMissingPositiveConst(Array(1,2,0)) should be (3)
    FirstMissingPositive.firstMissingPositiveConst(Array(3,4,-1,1)) should be (2)
    FirstMissingPositive.firstMissingPositiveConst(Array(7,8,9,11,12)) should be (1)
    FirstMissingPositive.firstMissingPositiveConst(Array()) should be (1)
    FirstMissingPositive.firstMissingPositiveConst(Array(1)) should be (2)
    FirstMissingPositive.firstMissingPositiveConst(Array(1, 1)) should be (2)
    FirstMissingPositive.firstMissingPositiveConst(Array(2, 2)) should be (1)
  }
}
