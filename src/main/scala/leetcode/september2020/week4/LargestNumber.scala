package leetcode.september2020.week4

object LargestNumber {
  def largestNumber(nums: Array[Int]): String = {
    val res = largest(nums.toList.map(_.toString), 9, "")
    if (res.forall(_ == '0')) "0"
    else res
  }

  def largest(nums: List[String], digit: Int, result: String): String = {
    //    println(digit, result, nums)
    if (nums.isEmpty) {
      result
    } else if (digit == 0) {
      result + nums.mkString("")
    } else {
      val (candidates, less) = nums.partition(_.startsWith(digit.toString))
      val sorted = candidates.sorted(largestOrdering)
      largest(less, digit - 1, result + sorted.mkString(""))
    }
  }

  val largestOrdering: Ordering[String] = (a: String, b: String) => {
    if (a.length == b.length) {
      b.compareTo(a)
    } else {
      (b + a).compareTo(a + b)
    }
  }

  implicit class ListRemove[T](list: List[T]) {
    def remove(elem: T): List[T] = {
      val i = list.indexOf(elem)
      if (i == -1) {
        list
      } else {
        val (before, after) = list.splitAt(i)
        before ++ after.drop(1)
      }
    }
  }

}
