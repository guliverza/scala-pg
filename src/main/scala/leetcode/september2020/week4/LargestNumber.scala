package leetcode.september2020.week4

object LargestNumber {
  def largestNumber(nums: Array[Int]): String = {
    val res = nums.map(_.toString).sorted(largestOrdering).mkString("")
    if (res.forall(_ == '0')) "0"
    else res
  }

  val largestOrdering: Ordering[String] = (a: String, b: String) => {
    if (a.length == b.length) {
      b.compareTo(a)
    } else {
      (b + a).compareTo(a + b)
    }
  }
}
