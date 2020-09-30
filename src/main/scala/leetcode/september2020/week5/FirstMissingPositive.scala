package leetcode.september2020.week5

object FirstMissingPositive {
  def firstMissingPositive(nums: Array[Int]): Int = firstMissingPositiveBitSet(nums)

  def firstMissingPositiveConst(nums: Array[Int]): Int = {
    val length = nums.length
    for {i <- nums.indices} {
      if (nums(i) > 0 && nums(i) <= length && nums(i) - 1 != i) {
        val value = nums(nums(i) - 1)
        nums(nums(i) - 1) = nums(i)
        nums(i) = value
      }
      println(nums.mkString(","))
    }
    nums.zipWithIndex.find { case (n, i) => n - 1 != i }.map { case (n, i) => i }.getOrElse(nums.length) + 1
  }

  def firstMissingPositiveBitSet(nums: Array[Int]): Int = {
    val length = nums.length
    val missed = nums.foldLeft(collection.BitSet.fromSpecific(0 to length)) {
      case (set, n) => if (n > 0 && n <= length) set - (n - 1) else set
    }
    missed.headOption.getOrElse(0) + 1
  }

  def firstMissingPositiveArray(nums: Array[Int]): Int = {
    val array = Array.fill[Boolean](nums.length + 1)(false)
    nums.foreach(n => if (n > 0 && n <= array.length) array(n - 1) = true)
    array.indexOf(false) + 1
  }
}