package leetcode

object ClosestPalindrome {
  /**
   * Given an integer n, find the closest integer (not including itself), which is a palindrome.
   *
   * The 'closest' is defined as absolute difference minimized between two integers.
   *
   * Example 1:
   *
   * Input: "123"
   * Output: "121"
   */

  def nearestPalindromic(n: String): String = {
    val input = n.toLong
    val leftWithMiddle = n.take(math.round(n.length / 2.0f)).toLong
    val haveMiddle = n.length % 2 == 1
    val palindroms1 = for {
      left <- leftWithMiddle - 1 to leftWithMiddle + 1
    } yield {
      val right = (if (haveMiddle) left / 10 else left).toString.reverse
      (left + right).toLong
    }
    val palindroms2 = (-2 to 2).collect {
      case x if input + x >= 0 => input + x
    } map (x => mirror(x.toString).toLong)
    val best = (palindroms1 ++ palindroms2).fold(input) {
      case (res, `input`) => res
      case (`input`, res1) => res1
      case (res, res1) =>
        val diff = math.abs(res - input)
        val diff1 = math.abs(res1 - input)
        if (diff1 < diff || (diff1 == diff && res1 < input)) res1
        else res
    }
    best.toString
  }

  def mirror(n: String): String = {
    val left = n.take(n.length / 2)
    val newRight = n.take(n.length / 2).reverse
    val middle = if (n.length % 2 == 0) ""
    else n(n.length / 2)
    left + middle + newRight
  }
}
