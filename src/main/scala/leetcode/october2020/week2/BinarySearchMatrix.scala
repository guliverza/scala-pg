package leetcode.october2020.week2

object BinarySearchMatrix {
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    val m = matrix.headOption.map(_.length).getOrElse(0)
    val n = matrix.length

    def get(i: Int) = if (m == 1) matrix(i).head else matrix(i / m)(i % m)

    @scala.annotation.tailrec
    def binSearch(from: Int, to: Int): Boolean = {
      (to >= from) && {
        val mid = from + (to - from) / 2
        val midNumber = get(mid)
        if (midNumber == target) {
          true
        } else if (target < midNumber) {
          binSearch(from, mid - 1)
        } else {
          binSearch(mid + 1, to)
        }
      }
    }

    m > 0 && n > 0 && binSearch(0, m * n - 1)
  }

  def main(args: Array[String]): Unit = {
    println(searchMatrix(Array(Array(1, 3, 5, 7), Array(10, 11, 16, 20), Array(23, 30, 34, 50)), 7))
    println(searchMatrix(Array(Array(1, 1)), 0))
    println(searchMatrix(Array(Array(1, 3)), 3))
    println(searchMatrix(Array(Array(1), Array(3)), 3))
  }
}
