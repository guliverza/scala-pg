package leetcode.october2020.week3

object RotateArrayRight {
  def rotate(nums: Array[Int], k: Int): Unit = {
    val len = nums.length
    val shift = k % len
    if (shift != 0 && len > 1) {
      reverse(nums, 0, len - 1)
      reverse(nums, 0, shift - 1)
      reverse(nums, shift, len - 1)
    }
  }

  def reverse[T](a: Array[T], from: Int, to: Int): Unit = {
    var left = from
    var right = to
    while (left < right) {
      val t = a(left)
      a(left) = a(right)
      a(right) = t
      left += 1
      right -= 1
    }
  }
}
