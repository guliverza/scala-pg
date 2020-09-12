package leetcode.september2020.week2

object CombinationSumIII {
  /**
   * Find all possible combinations of k numbers that add up to a number n,
   * given that only numbers from 1 to 9 can be used and each combination should be a unique set of numbers.
   *
   * Note:
   * All numbers will be positive integers.
   * The solution set must not contain duplicate combinations.
   */

  def combinationSum3(k: Int, n: Int): List[List[Int]] = {
    (1 to math.min(9, n - (1 until k).sum)).toList.combinations(k).filter(_.sum == n).toList
  }
}
