package leetcode.september2020.week3

object SequentialDigits {
  /**
   * An integer has sequential digits if and only if each digit in the number is one more than the previous digit.
   *
   * Return a sorted list of all the integers in the range [low, high] inclusive that have sequential digits.
   */
  def sequentialDigits(low: Int, high: Int): List[Int] = {
    new SequentialBuilder(low, high).toList
  }

  class SequentialBuilder(val from: Int = 10, val to: Int) extends Iterator[Int] {
    var (currentNumber, currentStep) = {
      val s = from.toString
      val (firstDigit, len) = if (s.take(1).toInt + s.length <= 10) {
        (s.take(1).toInt, s.length)
      } else {
        (1, s.length + 1)
      }
      val n = makeNumber(firstDigit, len)
      val step = makeStep(len)
      if (n >= from) {
        (n, step)
      } else {
        calcNext(n, step)
      }
    }

    override def hasNext: Boolean = currentNumber <= to && currentNumber != Int.MaxValue

    override def next(): Int = {
      val result = currentNumber
      val (next, nextStep) = calcNext(currentNumber, currentStep)
      currentNumber = next
      currentStep = nextStep
      result
    }

    private def calcNext(n: Int, step: Int): (Int, Int) = {
      if (n % 10 <= 8) {
        (n + step, step)
      } else {
        val len = n.toString.length + 1
        (makeNumber(1, len), makeStep(len))
      }
    }

    def makeNumber(from: Int, length: Int): Int = if (length < 10)
      (from until (from + length)).mkString("").toInt
    else
      Int.MaxValue

    def makeStep(len: Int): Int = ("1" * len).mkString("").toInt
  }

}
