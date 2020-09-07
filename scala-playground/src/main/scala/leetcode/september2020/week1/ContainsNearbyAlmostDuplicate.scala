package leetcode.september2020.week1

object ContainsNearbyAlmostDuplicate {

  def containsNearbyAlmostDuplicate(nums: Array[Int], k: Int, t: Int): Boolean = {
    (for {
      i <- nums.indices
      j <- math.max(0, i - k) until math.min(i + k, nums.length)
      if i != j
      if math.abs(nums(i).toLong - nums(j)) <= t.toLong
    } yield {
      println(s"($i, $j) => ${math.abs(nums(i))}")
      println(s"($i, $j) => ${math.abs(nums(j))}")
      println(s"($i, $j) => ${math.abs(math.abs(nums(j)) - math.abs(nums(i)))}")
      i -> j
    })
      .headOption.nonEmpty
  }

}
