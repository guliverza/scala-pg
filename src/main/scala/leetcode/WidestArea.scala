package leetcode

object WidestArea {
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val sortedX: List[Int] = points.toList.map(_.head).sorted
    val pairs = sortedX zip sortedX.tail
    (pairs map { case (a, b) => b - a }).max
  }
}
