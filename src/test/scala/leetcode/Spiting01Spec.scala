package leetcode

import leetcode.Spliting01.maxScore
import org.scalatest.flatspec._
import org.scalatest.matchers._

class Spiting01Spec extends AnyFlatSpec with should.Matchers {
  "Spiting01" must "find" in {
    maxScore("011101") shouldBe 5
    maxScore("00111") shouldBe 5
    maxScore("1111") shouldBe 3
    maxScore("11111") shouldBe 4
    maxScore("01111") shouldBe 5
    maxScore("01") shouldBe 2
    maxScore("10") shouldBe 0
  }
}
