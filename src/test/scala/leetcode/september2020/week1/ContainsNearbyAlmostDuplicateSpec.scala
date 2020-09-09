package leetcode.september2020.week1

import leetcode.september2020.week1.ContainsNearbyAlmostDuplicate.containsNearbyAlmostDuplicate
import org.scalatest._
import flatspec._
import matchers._

class ContainsNearbyAlmostDuplicateSpec extends AnyFlatSpec with should.Matchers {
  "ContainsNearbyAlmostDuplicate" must
    "..." in {
    containsNearbyAlmostDuplicate(Array(-2147483648, 2147483647), k = 1, t = 1) shouldBe false
  }
}
