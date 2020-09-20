package leetcode.september2020.week3

object BestDiff {
  /**
   * If you were only permitted to complete at most one transaction
   * (i.e., buy one and sell one share of the stock), design an algorithm to find the maximum profit.
   *
   * Note that you cannot sell a stock before you buy one.
   */
  def maxProfit(prices: Array[Int]): Int = {
    if (prices.length >= 2) {
      val min = prices.take(2).min
      val max = prices(1)
      val (bestMin, bestMax, tmpMin) = prices.drop(2).foldLeft((min, max, min)) {
        case ((min, max, tmpMin), price) =>
          if (price < min && (max - min > price - tmpMin)) {
            (min, max, math.min(tmpMin, price))
          } else if (price > max || price - tmpMin > max - min) {
            (tmpMin, price, tmpMin)
          } else {
            (min, max, tmpMin)
          }
      }
      bestMax - bestMin
    } else {
      0
    }
  }

}
