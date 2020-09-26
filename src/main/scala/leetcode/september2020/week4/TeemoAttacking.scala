package leetcode.september2020.week4

object TeemoAttacking {
  def findPoisonedDuration(timeSeries: Array[Int], duration: Int): Int = {
    timeSeries.toList match {
      case Nil => 0
      case head :: Nil => duration
      case head :: tail =>
        val (_, total) = tail.foldLeft((head, duration)) {
          case ((prev, sum), t) => (t, sum + math.min(duration, t - prev))
        }
        total
    }
  }
}
