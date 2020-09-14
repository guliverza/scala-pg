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
    HouseRobber.rob(Array(1,3,1,3,1,3,1,3,1,3,100)) shouldBe 112
    HouseRobber.rob(Array(1,3,1,3,1,3,1,3,1,3,1,3,100)) shouldBe 115
    HouseRobber.rob(Array(1,3,1,3,1,3,1,3,1,3,1,3,1,3,100)) shouldBe 118
    HouseRobber.rob(Array(1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,100)) shouldBe 121
    HouseRobber.rob(Array(1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,100)) shouldBe 124
    HouseRobber.rob(Array(1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,100)) shouldBe 127
    HouseRobber.rob(Array(1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,100)) shouldBe 130
    HouseRobber.rob(Array(1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,100)) shouldBe 133
    HouseRobber.rob(Array(1,3,1,4,1,5,1,6,1,7,1,8,1,9,1,10,1,11,1,12,1,13,100)) shouldBe 175
    HouseRobber.rob(Array(1,1,3,6,7,10,7,1,8,5,9,1,4,4,3)) shouldBe 42
    HouseRobber.rob(Array(183,219,57,193,94,233,202,154,65,240,97,234,100,249,186,66,90,238,168,128,177,235,50,81,185,165,217,207,88,80,112,78,135,62,228,247,211)) shouldBe 3365
    HouseRobber.rob(Array(114,117,207,117,235,82,90,67,143,146,53,108,200,91,80,223,58,170,110,236,81,90,222,160,165,195,187,199,114,235,197,187,69,129,64,214,228,78,188,67,205,94,205,169,241,202,144,240)) shouldBe 4173
    HouseRobber.rob(Array(226,174,214,16,218,48,153,131,128,17,157,142,88,43,37,157,43,221,191,68,206,23,225,82,54,118,111,46,80,49,245,63,25,194,72,80,143,55,209,18,55,122,65,66,177,101,63,201,172,130,103,225,142,46,86,185,62,138,212,192,125,77,223,188,99,228,90,25,193,211,84,239,119,234,85,83,123,120,131,203,219,10,82,35,120,180,249,106,37,169,225,54,103,55,166,124)) shouldBe 7102
  }

}
