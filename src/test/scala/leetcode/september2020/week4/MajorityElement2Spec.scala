package leetcode.september2020.week4

import org.scalatest.flatspec._
import org.scalatest.matchers._

class MajorityElement2Spec extends AnyFlatSpec with should.Matchers {
  "MajorityElement2" should "find majority elements" in {
    MajorityElement2.majorityElement(Array(4,2,1,1)) should be (List(1))
    MajorityElement2.majorityElement(Array(0,3,4,0)) should be (List(0))
    MajorityElement2.majorityElement(Array(1,2,2,3,3,4,4,5,1,1,1,1)) should be (List(1))
  }

}
