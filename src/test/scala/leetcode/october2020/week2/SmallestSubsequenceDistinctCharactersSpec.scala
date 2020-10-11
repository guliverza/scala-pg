package leetcode.october2020.week2

import org.scalatest.flatspec._
import org.scalatest.matchers._

class SmallestSubsequenceDistinctCharactersSpec extends AnyFlatSpec with should.Matchers {
  "SmallestSubsequenceDistinctCharacters" should "a" in {
    SmallestSubsequenceDistinctCharacters.smallestSubsequence("bccab") should be("bca")
    SmallestSubsequenceDistinctCharacters.smallestSubsequence("bcabc") should be("abc")
    SmallestSubsequenceDistinctCharacters.smallestSubsequence("cbacdcbc") should be("acdb")
    SmallestSubsequenceDistinctCharacters.smallestSubsequence("bcabc") should be("abc")
    SmallestSubsequenceDistinctCharacters.smallestSubsequence("cdadabcc") should be("adbc")
    SmallestSubsequenceDistinctCharacters.smallestSubsequence("thesqtitxyetpxloeevdeqifkz") should be("hesitxyplovdqfkz")
  }
}
