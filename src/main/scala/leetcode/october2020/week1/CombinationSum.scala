package leetcode.october2020.week1

object CombinationSum {
  def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {
    candidates.collect {
      case n if n == target =>
        List(List(n))
      case n if n < target =>
        combinationSum(candidates.dropWhile(_ != n), target - n).map(l => n :: l)
    }.fold(Nil)(_ ++ _)
  }
}
