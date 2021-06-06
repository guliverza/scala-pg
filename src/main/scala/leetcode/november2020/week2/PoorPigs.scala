package leetcode.november2020.week2

object PoorPigs {
  def poorPigs(buckets: Int, minutesToDie: Int, minutesToTest: Int): Int = {
    val t = minutesToTest / minutesToDie
    math.ceil(log(t+1, buckets)).toInt
  }

  def log(base: Double, x: Double): Double = math.log(x)/math.log(base)

  def main(args: Array[String]): Unit = {
    println(poorPigs(8, 15, 15))
  }
}
