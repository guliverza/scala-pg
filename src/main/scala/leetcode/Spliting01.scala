package leetcode

object Spliting01 {
  def maxScore(s: String): Int = {
    val left = s.take(1).count(_ == '0')
    val right = s.tail.count(_ == '1')
    val (_, _, best) = s.tail.foldLeft((left, right, 0)) {
      case ((left, right, best), c) =>
        val (newLeft, newRight) = if (c == '1') (left, right - 1) else (left + 1, right)
        val newBest = Math.max(left + right, best)
        (newLeft, newRight, newBest)
    }
    best
  }
}
