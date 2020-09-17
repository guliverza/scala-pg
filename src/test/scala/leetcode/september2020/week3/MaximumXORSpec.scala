package leetcode.september2020.week3

import org.scalatest.flatspec._
import org.scalatest.matchers._

class MaximumXORSpec extends AnyFlatSpec with should.Matchers {

  "MaximumXOR" should "find maximum of 2 numbers" in {
    MaximumXOR.findMaximumXOR(Array(3, 10, 5, 25, 2, 8)) should be (28)
    MaximumXOR.findMaximumXOR(Array(8, 10, 2)) should be (10)
    MaximumXOR.findMaximumXOR(Array(2147483647,2147483646)) should be (1)
    MaximumXOR.findMaximumXOR(Array(1,1)) should be (0)
  }

}
