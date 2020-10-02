package codewars

object Sol {
  def seriesSum(n: Int): String = {
    val sum = (n to 1 by -1).map(f).sum
    f"$sum%.2f"
  }

  def f(n: Int): Double = 1.0 / (3 * n - 2)
}
