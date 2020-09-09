package leetcode

import leetcode.StrongPasswordChecker.strongPasswordChecker
import org.scalatest._
import flatspec._
import matchers._
class StrongPasswordCheckerSpec extends AnyFlatSpec with should.Matchers {
  "StrongPasswordChecker" must "check pass" in {
    strongPasswordChecker("1111111111") shouldBe 3
    strongPasswordChecker("1234567890123456Baaaaa") shouldBe 3
    strongPasswordChecker("ABABABABABABABABABAB1") shouldBe 2
    strongPasswordChecker("aaaaaaaaaaaaaaaaaaaaa") shouldBe 7
    strongPasswordChecker("aaa123") shouldBe 1
  }
}
