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

  val RobPattern = List((0, 2, 4), (0, 3, 5), (0, 2, 5), (0, 3, 6))

  def rob(nums: Array[Int]): Int = {
    def getOrZero(i: Int) = if (i < nums.length) nums(i) else 0

    val (sum, _) = nums.zipWithIndex.foldLeft((0, 1)) {
      case ((sum, skipped), (current, i)) if skipped > 0 =>
        val withCurrent = RobPattern.map { case (r1, r2, r3) =>
          getOrZero(i + r1) + getOrZero(i + r2) + getOrZero(i + r3) }.max
        val woCurrent = RobPattern.map { case (r1, r2, r3) =>
          getOrZero(i + r1 + 1) + getOrZero(i + r2 + 1) + getOrZero(i + r3 + 1) }.max
        if (withCurrent >= woCurrent || skipped  >= 2) {
          (sum + current, 0)
        } else {
          (sum, skipped + 1)
        }
      case ((sum, skipped), _) =>
        (sum, skipped + 1)
    }
    sum
  }

}
