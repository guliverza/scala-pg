package leetcode.september2020.week1

object LargestOverlap {
  def largestOverlap(A: Array[Array[Int]], B: Array[Array[Int]]): Int = {
    val sizeX = A(0).length
    val sizeY = A.length

    def count(A: Array[Array[Int]], B: Array[Array[Int]], x: Int, y: Int): Int = {
      (for {
        i <- 0 until sizeX - x
        j <- 0 until sizeY - y
      } yield A(i + x)(j + y) + B(i)(j)).count(_ == 2)
    }

    (for {
      x <- 0 until sizeX
      y <- 0 until sizeY
    } yield {
      math.max(count(A, B, x, y), count(B, A, x, y))
    }).max

  }

  def strongPasswordChecker(s: String): Int = {
    Seq(
      s.length >= 6 && s.length <= 20,
      s.exists(c => c.isLetter && c.isLower) && s.exists(c => c.isLetter && c.isUpper),
      s.exists(c => c.isDigit)
    ).count(!_)
  }

}