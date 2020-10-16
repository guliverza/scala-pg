package leetcode.october2020.week2

object BinarySearch {
  def search(nums: Array[Int], target: Int): Int = {
    @scala.annotation.tailrec
    def binSearch(from: Int, to: Int): Int = {
      if (to >= from) {
        val mid = from + (to - from) / 2
        if (nums(mid) == target) {
          mid
        } else if (target < nums(mid)) {
          binSearch(from, mid - 1)
        } else {
          binSearch(mid + 1, to)
        }
      } else {
        -1
      }
    }

    binSearch(0, nums.length - 1)
  }

  def main(args: Array[String]): Unit = {
    println(search(Array(1, 3, 4, 5, 6, 7, 8, 9, 10, 11), 2))
    println(search(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 0))
    println(search(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 11))
    println(search(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 1))
    println(search(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 10))
  }
}
