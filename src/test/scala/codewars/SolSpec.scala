package codewars

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SolSpec extends AnyFlatSpec with should.Matchers {
  Sol.seriesSum(1) should be("1.0")
  Sol.seriesSum(2) should be("1.25")
  Sol.seriesSum(3) should be("1.3928571428571428")
  Sol.seriesSum(100000000) should be("7.184238174101984") // n to 1 by -1
//Sol.seriesSum(100000000) should be("7.18423817410127")  // 1 to n
}
