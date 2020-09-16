package leetcode.september2020.week3

import org.scalatest.flatspec._
import org.scalatest.matchers._

class MaximumXORSpec extends AnyFlatSpec with should.Matchers {

  "MaximumXOR" should "find maximum of 2 numbers" in {
    MaximumXOR.findMaximumXOR(Array(3, 10, 5, 25, 2, 8)) should be (28)
  }

}
