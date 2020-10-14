package leetcode.october2020.week2

import leetcode.september2020.week2.HouseRobber

object HouseRobber2 {
  def rob(nums: Array[Int]): Int = nums match {
    case Array() => 0
    case Array(x) => x
    case Array(_, _) => nums.max
    case Array(_, _, _) => nums.max
    case _ => math.max(HouseRobber.rob(nums.init), HouseRobber.rob(nums.tail))
  }
}
