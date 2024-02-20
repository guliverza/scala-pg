package leetcode

import org.scalatest.flatspec._
import org.scalatest.matchers._

class TrappingRainWaterSpec extends AnyFlatSpec with should.Matchers {
  "TrappingRainWater" must "trap" in {
    TrappingRainWater.trap(Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)) shouldBe 6
    TrappingRainWater.trap(Array(4, 2, 0, 3, 2, 5)) shouldBe 9
  }
}
