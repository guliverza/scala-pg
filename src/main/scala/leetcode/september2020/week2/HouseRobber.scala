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

  def rob(nums: Array[Int]): Int = {
    if (nums.length == 0) {
      0
    } else if (nums.length <= 2) {
      nums.max
    } else {
      val (_, dp2) = nums.drop(2).foldLeft((nums(0), nums.take(2).max)) {
        case ((dp0, dp1), num) => (dp1, math.max(dp0 + num, dp1))
      }
      dp2
    }
  }

  /**
   * Slow brute force version
   */
  def robFullSearch(moneys: List[Int]): Int = {
    moneys match {
      case Nil => 0
      case head :: Nil => head
      case (head :: second :: Nil) => math.max(head, second)
      case (head :: second :: tail) => math.max(head + robFullSearch(tail), second + robFullSearch(tail.tail))
    }
  }
}
