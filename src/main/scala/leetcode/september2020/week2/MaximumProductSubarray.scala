package leetcode.september2020.week2

object MaximumProductSubarray {
  /**
   * Given an integer array nums, find the contiguous subarray within an array (containing at least one number) which has the largest product.
   */
  def maxProduct(nums: Array[Int]): Int = {
    val (_, _, max, visited) = nums.foldLeft((1, 1, nums.head, false)) {
      case ((maxEndingHere, minEndingHere, max, visited), n) =>
        if (n > 0) {
          (maxEndingHere * n, math.min(1, minEndingHere * n), math.max(max, maxEndingHere * n), true)
        } else if (n == 0) {
          (1, 1, math.max(max, maxEndingHere * n), visited)
        } else {
          (math.max(1, minEndingHere * n), maxEndingHere * n, math.max(max, minEndingHere * n), true)
        }
    }
    if (visited) {
      max
    } else {
      nums.max
    }
  }
}
