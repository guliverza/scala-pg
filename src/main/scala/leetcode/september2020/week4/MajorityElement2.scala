package leetcode.september2020.week4

object MajorityElement2 {
  /**
   * Given an integer array of size n, find all elements that appear more than ⌊ n/3 ⌋ times.
   * Note: The algorithm should run in linear time and in O(1) space.
   */

  // O(n) space
  def majorityElement(nums: Array[Int]): List[Int] = {
    nums.groupBy(num => num)
      .collect { case (num, thisNums) if thisNums.length > nums.length / 3 => num }
      .toList
  }

  def majorityElement1(nums: Array[Int]): Int = {
    val (candidate, _) = nums.foldLeft((0, 0)) {
      case ((candidate, candidateCount), a) =>
        if (candidateCount == 0) {
          (a, 1)
        } else if (candidate == a) {
          (a, candidateCount + 1)
        } else {
          (candidate, candidateCount - 1)
        }
    }
    candidate
  }

  def majorityElement2(nums: Array[Int]): List[Int] = {
    if (nums.length <= 2) {
      nums.toSet.toList
    } else {
      val (a, countA, b, countB) = nums.foldLeft((0, 0, 0, 0)) {
        case ((a, countA, b, countB), n) =>
          val res = if (a == n) {
            (n, countA + 1, b, countB)
          } else if (b == n) {
            (a, countA, n, countB + 1)
          } else if (countA == 0) {
            (n, 1, b, countB)
          } else if (countB == 0) {
            (a, countA, n, 1)
          } else {
            (a, countA - 1, b, countB - 1)
          }
          println(s"$n => $res")
          res
      }
      List((a, countA), (b, countB))
        .collect { case (a, _) if nums.count(_ == a) > nums.length/3 => a }
        .distinct
    }
  }

  def majorityElement3(nums: Array[Int]): List[Int] = {
    if (nums.length <= 2) {
      nums.toSet.toList
    } else {
      val (cand1, cand2, cand3, count1, count2, count3) = nums.foldLeft((0, 0, 0, 0, 0, 0)) {
        case ((cand1, cand2, cand3, count1, count2, count3), a) =>
          if (cand1 == a) {
            (a, cand2, cand3, count1 + 1, count2, count3)
          } else if (cand2 == a) {
            (cand1, a, cand3, count1, count2 + 1, count3)
          } else if (cand3 == a) {
            (cand1, cand2, a, count1, count2, count3 + 1)
          } else if (count1 <= 1 & count2 > 0 && count3 > 0) {
            (a, cand2, cand3, 1, count2, count3)
          } else if (count2 <= 1 && count3 > 0) {
            (cand1, a, cand3, count1, 1, count3)
          } else if (count3 <= 1) {
            (cand1, cand2, a, count1, count2, 1)
          } else if (count1 <= count2 && count1 <= count3) {
            (cand1, cand2, cand3, count1 - 1, count2, count3)
          } else if (count2 <= count1 && count2 <= count3) {
            (cand1, cand2, cand3, count1, count2 - 1, count3)
          } else {
            (cand1, cand2, cand3, count1, count2, count3 - 1)
          }
      }
      val candidates = List((cand1, count1), (cand2, count2), (cand3, count3))
      println(candidates)
      candidates.collect { case (cand, count) if count > nums.length/3 => cand }
    }
  }

}
