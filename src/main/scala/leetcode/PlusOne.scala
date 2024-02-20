package leetcode

object PlusOne {
  def plusOne(digits: Array[Int]): Array[Int] = {
    val (res, p) = digits.foldRight((List[Int](), 1)) {
      case (digit, (res, 1)) if digit == 9 => (0 :: res, 1)
      case (digit, (res, 1)) => ((digit + 1) :: res, 0)
      case (digit, (res, 0)) => (digit :: res, 0)
    }
    if (p == 1) {
      (1 :: res).toArray
    } else {
      res.toArray
    }
  }
}
