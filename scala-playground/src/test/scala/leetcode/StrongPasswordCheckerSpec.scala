package leetcode

import leetcode.StrongPasswordChecker.strongPasswordChecker
import org.scalatest._
class StrongPasswordCheckerSpec extends FlatSpec with Matchers {
  "StrongPasswordChecker" must
    "check pass" in {
    strongPasswordChecker("1111111111") shouldBe 3
  }
}
