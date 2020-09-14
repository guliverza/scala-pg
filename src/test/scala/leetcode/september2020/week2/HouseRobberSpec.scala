package leetcode.september2020.week2

import org.scalatest.flatspec._
import org.scalatest.matchers._

class HouseRobberSpec extends AnyFlatSpec with should.Matchers {

  "House robber" should "rob maximum without alerting the police." in {
    HouseRobber.rob(Array(1,2,3,1)) shouldBe 4
    HouseRobber.rob(Array(2,7,9,3,1)) shouldBe 12
    HouseRobber.rob(Array(2,7,3,9,1)) shouldBe 16
    HouseRobber.rob(Array(1,3,1,3,100)) shouldBe 103
    HouseRobber.rob(Array(1,3,1,3,1,3,1,3,100)) shouldBe 109
//    HouseRobber.rob(Array(1,1,3,6,7,10,7,1,8,5,9,1,4,4,3)) shouldBe 42

  }

}
