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

  val RobPatterns = Seq(
    Seq(0, 2, 4, 6, 8, 10),
    Seq(0, 2, 4, 6, 9),
    Seq(0, 2, 4, 7, 9),
    Seq(0, 2, 4, 7, 10),
    Seq(0, 2, 5, 7, 9),
    Seq(0, 2, 5, 7, 10),
    Seq(0, 2, 5, 8, 10),
    Seq(0, 3, 5, 7, 9),
    Seq(0, 3, 5, 7, 10),
    Seq(0, 3, 5, 8, 10),
    Seq(0, 3, 6, 8, 10),
    Seq(0, 3, 6, 9))
  val max = RobPatterns.flatten.max

  def rob(nums: Array[Int]): Int = {
    def getOrZero(i: Int) = if (i < nums.length) nums(i) else 0

    val (sum, _) = nums.zipWithIndex.foldLeft((0, 1)) {
      case ((sum, skipped), (current, i)) if skipped > 0 =>
        val withCurrent = RobPatterns.map(_.map { house => getOrZero(i + house) }.sum).max
        val woCurrent = RobPatterns.map(_.collect { case house if house < max => getOrZero(i + house + 1) }.sum).max
        if (withCurrent >= woCurrent) {
          (sum + current, 0)
        } else {
          (sum, skipped + 1)
        }
      case ((sum, skipped), _) =>
        (sum, skipped + 1)
    }
    sum
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
