package leetcode.october2020.week1

object KPairsInArray {
  def findPairs(nums: Array[Int], k: Int): Int = {
    val pairs = for {
      i <- nums.indices
      j <- (i + 1) until nums.length
      if math.abs(nums(i) - nums(j)) == k
    } yield Set(nums(i), nums(j))
//    println(pairs.distinct)
    pairs.distinct.size
  }
}
