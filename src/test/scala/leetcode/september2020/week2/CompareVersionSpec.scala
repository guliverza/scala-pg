package leetcode.september2020.week2

import org.scalatest._
import flatspec._
import matchers._

class CompareVersionSpec extends AnyFlatSpec with should.Matchers {
  "CompareVersion" should "compare" in {
    CompareVersion.compareVersion("1.2.3", "1.2.4") should be (-1)
    CompareVersion.compareVersion("1.2.3", "1.2.3.1") should be (-1)
    CompareVersion.compareVersion("1.2.3.1", "1.2.3") should be (1)
    CompareVersion.compareVersion("1.0", "1") should be (0)
    CompareVersion.compareVersion("1", "1.0") should be (0)
  }

}
