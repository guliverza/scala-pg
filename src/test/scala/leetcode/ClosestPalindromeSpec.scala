package leetcode

import leetcode.ClosestPalindrome.nearestPalindromic
import org.scalatest._
import flatspec._
import matchers._
class ClosestPalindromeSpec extends AnyFlatSpec with should.Matchers {
  "ClosestPalindrome" must "find lines" in {
    nearestPalindromic("123") should be("121")
    nearestPalindromic("1") should be("0")
    nearestPalindromic("230") should be("232")
    nearestPalindromic("987099999") should be("987101789")
    nearestPalindromic("11") should be("9")
    nearestPalindromic("11011") should be("11111")
    nearestPalindromic("358764513820540928") should be("358764513315467853")
    nearestPalindromic("1837722381") should be("1837667381")
    nearestPalindromic("99") should be("101")
    nearestPalindromic("999") should be("1001")
    nearestPalindromic("998") should be("999")
    nearestPalindromic("997") should be("999")
    nearestPalindromic("996") should be("999")
  }
}
