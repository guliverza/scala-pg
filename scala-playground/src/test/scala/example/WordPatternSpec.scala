package example
import org.scalatest._

class WordPatternSpec extends FlatSpec with Matchers{
  "WordPattern" should "find patterns" in {
    WordPattern.wordPattern("abba", "dog dog dog dog") shouldBe false
    WordPattern.wordPattern("abba", "dog cat cat dog") shouldBe true
  }

}
