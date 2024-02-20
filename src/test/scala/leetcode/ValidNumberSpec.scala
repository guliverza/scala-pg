package leetcode

import org.scalatest.flatspec._
import org.scalatest.matchers._

class ValidNumberSpec extends AnyFlatSpec with should.Matchers {
  "ValidNumber" must "check" in {
    ValidNumber.isNumber("1") shouldBe true
    ValidNumber.isNumber("44e016912630333") shouldBe true
    ValidNumber.isNumber("-123.456e789") shouldBe true
    ValidNumber.isNumber("-0.1") shouldBe true
    ValidNumber.isNumber("+6e-1") shouldBe true
    ValidNumber.isNumber("3e+7") shouldBe true
    ValidNumber.isNumber("-90E3") shouldBe true
    ValidNumber.isNumber("-.9") shouldBe true
    ValidNumber.isNumber("4.") shouldBe true
    ValidNumber.isNumber("3.") shouldBe true

    ValidNumber.isNumber("1a") shouldBe false
    ValidNumber.isNumber("1e") shouldBe false
    ValidNumber.isNumber("e3") shouldBe false
    ValidNumber.isNumber("e3") shouldBe false
    ValidNumber.isNumber("99e2.5") shouldBe false
    ValidNumber.isNumber("--6") shouldBe false
    ValidNumber.isNumber("-+3") shouldBe false
    ValidNumber.isNumber("95a54e53") shouldBe false
    ValidNumber.isNumber(".") shouldBe false
    ValidNumber.isNumber("safsax") shouldBe false
    ValidNumber.isNumber("Infinity") shouldBe false
    ValidNumber.isNumber("NaN") shouldBe false
  }
}
