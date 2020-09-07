package example

import example.ContainsNearbyAlmostDuplicate.containsNearbyAlmostDuplicate
import org.scalatest._

class ContainsNearbyAlmostDuplicateSpec extends FlatSpec with Matchers{
  "ContainsNearbyAlmostDuplicate" should "..." in {
    containsNearbyAlmostDuplicate(Array(-2147483648, 2147483647), k = 1, t = 1) shouldBe false
  }

}
