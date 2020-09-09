package leetcode.september2020.week1

import org.scalatest._
import flatspec._
import matchers._

class WordPatternSpec extends AnyFlatSpec with should.Matchers {
  "WordPattern" should "find patterns" in {
    WordPattern.wordPattern("abba", "dog dog dog dog") shouldBe false
    WordPattern.wordPattern("abba", "dog cat cat dog") shouldBe true
  }

}
