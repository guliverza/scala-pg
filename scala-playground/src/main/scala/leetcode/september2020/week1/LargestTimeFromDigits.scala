package leetcode.september2020.week1

import java.time.LocalTime
import java.time.format.DateTimeFormatter

object LargestTimeFromDigits {
  def largestTimeFromDigits(a: Array[Int]): String = {
    val allowed = a.permutations.collect {
      case Array(h1, h2, m1, m2) if h1 * 10 + h2 < 24 && m1 * 10 + m2 < 60 =>
        LocalTime.of(h1 * 10 + h2, m1 * 10 + m2)
    }
    if (allowed.isEmpty) ""
    else allowed.max.format(DateTimeFormatter.ofPattern("HH:mm"))
  }
}
