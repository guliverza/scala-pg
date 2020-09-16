package leetcode.september2020.week3

object MaximumXOR {
  def findMaximumXOR(nums: Array[Int]): Int = {
    nums.foldLeft(0) { case (max, n1) =>
      math.max(max, nums.foldLeft(0) {
        case (max2, n2) => math.max(max2, n1 ^ n2)
      })
    }
  }

}