package leetcode.september2020.week1

import leetcode.september2020.week1.ContainsNearbyAlmostDuplicate.containsNearbyAlmostDuplicate
import org.scalatest._

class ContainsNearbyAlmostDuplicateSpec extends FlatSpec with Matchers {
  "ContainsNearbyAlmostDuplicate" must
    "..." in {
      containsNearbyAlmostDuplicate(Array(-2147483648, 2147483647), k = 1, t = 1) shouldBe false
    }
}
