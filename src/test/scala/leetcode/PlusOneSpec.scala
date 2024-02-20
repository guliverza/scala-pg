package leetcode

import org.scalatest.flatspec._
import org.scalatest.matchers._

class PlusOneSpec extends AnyFlatSpec with should.Matchers {
  "PlusOne" must "addOne" in {
    PlusOne.plusOne(Array(1, 2, 3)) shouldBe Array(1, 2, 4)
    PlusOne.plusOne(Array(1, 9, 9)) shouldBe Array(2, 0, 0)
    PlusOne.plusOne(Array(9, 9, 9)) shouldBe Array(1, 0, 0, 0)
  }
}
