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
    HouseRobber.rob(Array(1,1,3,6,7,10,7,1,8,5,9,1,4,4,3)) shouldBe 42
    HouseRobber.rob(Array(183,219,57,193,94,233,202,154,65,240,97,234,100,249,186,66,90,238,168,128,177,235,50,81,185,165,217,207,88,80,112,78,135,62,228,247,211)) shouldBe 3365
//    HouseRobber.rob(Array(1,,3,,7,,7,,8,,9,,4,,3)) shouldBe 42
//    3,7,7    = 17
//    3,10,1   = 14
//    3,7,1    = 11
//    3,10,8   = 21
//
//    6,10,1,  = 17
//    6,7,8    = 21
//    6,7,5    = 18
//    6,10,8   = 24
  }

}
