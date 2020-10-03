package leetcode.october2020.week1

import org.scalatest.flatspec._
import org.scalatest.matchers._

class KPairsInArraySpec  extends AnyFlatSpec with should.Matchers {
  "KPairsInArray" should "find k-diff pairs in array" in {
    KPairsInArray.findPairs(Array(3,1,4,1,5), 2) should be (2)
    KPairsInArray.findPairs(Array(1,2,4,4,3,3,0,9,2,3), 3) should be (2)
  }

}
