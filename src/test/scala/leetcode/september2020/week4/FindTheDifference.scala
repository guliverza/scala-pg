package leetcode.september2020.week4

object FindTheDifference {
  def findTheDifference(s: String, t: String): Char = {
    t.toList.diff(s.toList).head
  }
}
