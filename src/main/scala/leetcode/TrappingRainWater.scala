package leetcode

object TrappingRainWater {

  def trap(height: Array[Int]): Int = {
    var level = 0
    var total = 0
    val max = height.max
    while (level < max) {
      val left = height.indexWhere(h => h > level)
      val right = height.lastIndexWhere(h => h > level)
      if (left < right) {
        val currentHeight = Math.min(height(left), height(right))
        for {i <- (left + 1) until right} {
          total += Math.max(0, currentHeight - Math.max(height(i), level))
        }
        level = currentHeight
      } else {
        level += 1
      }
    }
    total
  }
}
