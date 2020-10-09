package codewars

object Kata4 {
  def solution(xs: List[Int]): String = {
    val (ranges, to, from) = xs.foldLeft((List.empty[String], Option.empty[Int], Option.empty[Int])) {
      case ((ranges, Some(prev), Some(start)), n) if n - prev == 1 => (ranges, Some(n), Some(start))
      case ((ranges, Some(prev), Some(start)), n) => (ranges ++ createRange(start, prev) , Some(n), Some(n))
      case ((ranges, None, None), n) => (ranges, Some(n), Some(n))
    }
    val rest = for { left <- from; right <- to } yield createRange(left, right)
    (ranges ++ rest.getOrElse(Nil)).mkString(",")
  }

  def createRange(left: Int, right: Int): List[String] = {
    if (right - left >= 2) List(s"$left-$right")
    else (left to right).map(_.toString).toList
  }

  def main(args: Array[String]): Unit = {
    println(solution(List(1,2,3)))
    println(solution(List(-6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20)))
  }
}
