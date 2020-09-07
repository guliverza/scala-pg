package example
import example.LargestTimeFromDigits.largestTimeFromDigits
import org.scalatest._

class LargestTimeFromDigitsSpec extends FlatSpec with Matchers {
  "LargestTimeFromDigits" must
    "find max clock" in {
    largestTimeFromDigits(Array(1,2,3,4)) shouldBe "23:41"
    largestTimeFromDigits(Array(5,5,5,5)) shouldBe ""
    largestTimeFromDigits(Array(0,0,0,0)) shouldBe "00:00"
    largestTimeFromDigits(Array(0,0,1,0)) shouldBe "10:00"
  }
}
