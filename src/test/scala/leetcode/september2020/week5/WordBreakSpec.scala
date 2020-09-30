package leetcode.september2020.week5

import org.scalatest.flatspec._
import org.scalatest.matchers._

class WordBreakSpec extends AnyFlatSpec with should.Matchers {
  "WordBreak" should "find containing words" in {
    WordBreak.wordBreak("leetcode", List("leet", "code")) should be (true)
    WordBreak.wordBreak("applepenapple", List("apple", "pen")) should be (true)
    WordBreak.wordBreak("catsandog", wordDict = List("cats", "dog", "sand", "and", "cat")) should be (false)
    WordBreak.wordBreak("catskicatcats", List("cats","cat","dog","ski")) should be (true)
    WordBreak.wordBreak("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab",
      List("a","aa","aaa","aaaa","aaaaa","aaaaaa","aaaaaaa","aaaaaaaa","aaaaaaaaa","aaaaaaaaaa")) should be (false)
    WordBreak.wordBreak("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaabaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      List("aa","aaa","aaaa","aaaaa","aaaaaa","aaaaaaa","aaaaaaaa","aaaaaaaaa","aaaaaaaaaa","ba")) should be (false)

    WordBreak.reduceWords(List("aa","aaa","aaaa","aaaaa","aaaaaa","aaaaaaa","aaaaaaaa","aaaaaaaaa","aaaaaaaaaa","ba")) should be(List("aa", "aaa", "ba"))
  }
}
