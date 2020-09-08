package leetcode.september2020.week1

import leetcode.september2020.week1.LargestOverlap.largestOverlap
import org.scalatest._

class LargestOverlapSpec extends FlatSpec with Matchers {
  "LargestOverlap" must "..." in {
    val A = Array(
      Array(1, 1, 0),
      Array(0, 1, 0),
      Array(0, 1, 0))
    val B = Array(
      Array(0, 0, 0),
      Array(0, 1, 1),
      Array(0, 0, 1))
    largestOverlap(A, B) should be (3)
    val A1 = Array(Array(1))
    val B1 = Array( Array(1))
    largestOverlap(A1, B1) should be (1)
  }

}
