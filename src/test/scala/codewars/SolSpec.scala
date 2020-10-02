package codewars

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SolSpec extends AnyFlatSpec with should.Matchers {
  Sol.seriesSum(1) should be("1.00")
  Sol.seriesSum(2) should be("1.25")
  Sol.seriesSum(3) should be("1.39")
  Sol.seriesSum(100000000) should be("7.18")
}
