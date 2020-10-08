package leetcode.october2020.week2

object BinarySearch {
  def search(nums: Array[Int], target: Int): Int = {
    @scala.annotation.tailrec
    def binSearch(from: Int, to: Int): Int = {
      if (to > from) {
        val mid = from + (to - from) / 2
        if (nums(mid) == target) {
          mid
        } else if (target < nums(mid)) {
          binSearch(from, mid)
        } else {
          binSearch(mid, to)
        }
      } else if (from < nums.length && nums(from) == target) {
        from
      } else {
        -1
      }
    }

    binSearch(0, nums.length)
  }

  def main(args: Array[String]): Unit = {
    println(search(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 5))
    println(search(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 1))
    println(search(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 10))
    println(search(Array(1), 1))
    println(search(Array(1, 2), 2))
  }
}
