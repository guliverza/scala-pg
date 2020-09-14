package leetcode.september2020.week2

object HouseRobber {
  /**
   * You are a professional robber planning to rob houses along a street.
   * Each house has a certain amount of money stashed, the only constraint stopping you from robbing each of them
   * is that adjacent houses have security system connected and it will automatically contact the police
   * if two adjacent houses were broken into on the same night.
   *
   * Given a list of non-negative integers representing the amount of money of each house,
   * determine the maximum amount of money you can rob tonight without alerting the police.
   */

  val WithPatterns = Seq(Seq(0, 2, 4, 6), Seq(0, 3, 5), Seq(0, 2, 5), Seq(0, 3, 6))
  val WOPatterns = Seq(Seq(1, 3, 5), Seq(1, 4, 6), Seq(1, 3, 6))

  def rob(nums: Array[Int]): Int = {

    def getOrZero(i: Int) = if (i < nums.length) nums(i) else 0

    val (sum, _) = nums.zipWithIndex.foldLeft((0, false)) {
      case ((sum, false), (current, i)) =>
        def robPattern(patterns: Seq[Seq[Int]]) = patterns.map(_.map { house => getOrZero(i + house) }.sum).max

        val withCurrent = robPattern(WithPatterns)
        val woCurrent = robPattern(WOPatterns)
        if (withCurrent >= woCurrent) {
          (sum + current, true)
        } else {
          (sum, false)
        }
      case ((sum, true), _) =>
        (sum, false)
    }
    sum
  }

}
