package codewars

object SqInRect {

  def sqInRect(min: Int, max: Int): Array[Int] = {
    if (min == max || min == 0 || max == 0) Array()
    else if (min > max) sqInRect(max, min)
    else Array.fill(max/min)(min) ++ sqInRect(max % min, min)
  }
}