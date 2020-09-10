package leetcode.september2020.week2

import leetcode.september2020.week2.BullsAndCows.getHint
import org.scalatest.flatspec._
import org.scalatest.matchers._

class BullsAndCowsSpec extends AnyFlatSpec with should.Matchers {

  "Bulls and Cows" should "provide hints" in {
    getHint("1807", "7810") shouldBe "1A3B"
    getHint("1123", "0111") shouldBe "1A1B"
  }

}
