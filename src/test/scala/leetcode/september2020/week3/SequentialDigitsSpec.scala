package leetcode.september2020.week3

import org.scalatest.flatspec._
import org.scalatest.matchers._

class SequentialDigitsSpec extends AnyFlatSpec with should.Matchers {
  "SequentialDigits" should "build correct list" in {
    SequentialDigits.sequentialDigits(100, 300) should be(List(123, 234))
    SequentialDigits.sequentialDigits(1000, 13000) should be(List(1234,2345,3456,4567,5678,6789,12345))
    SequentialDigits.sequentialDigits(1300, 13000) should be(List(2345,3456,4567,5678,6789,12345))
    SequentialDigits.sequentialDigits(8511, 23553) should be(List(12345,23456))
    SequentialDigits.sequentialDigits(10, 1000000000) should be(List(12, 23, 34, 45, 56, 67, 78, 89, 123, 234, 345, 456, 567, 678, 789, 1234, 2345, 3456, 4567, 5678, 6789, 12345, 23456, 34567, 45678, 56789, 123456, 234567, 345678, 456789, 1234567, 2345678, 3456789, 12345678, 23456789, 123456789))
  }

}
