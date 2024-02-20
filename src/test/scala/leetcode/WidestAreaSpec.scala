package leetcode

import WidestArea.maxWidthOfVerticalArea
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class WidestAreaSpec extends AnyFlatSpec with should.Matchers {
  "WidestArea" must "find" in {
    maxWidthOfVerticalArea(Array(Array(8, 7), Array(9, 9), Array(7, 4), Array(9, 7))) shouldBe 1
    maxWidthOfVerticalArea(Array(Array(3, 1), Array(9, 0), Array(1, 0), Array(1, 4), Array(5, 3), Array(8, 8))) shouldBe 3
  }
}
