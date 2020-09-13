package leetcode.september2020.common

case class Interval(from: Int, to: Int) {
  def before(o: Interval): Boolean = to < o.from
  def after(o: Interval): Boolean = from > o.to
  def intersects(o: Interval): Boolean = !before(o) && !after(o)
  def asArray: Array[Int] = Array(from, to)
}
object Interval {
  def apply(a: Array[Int]): Interval = Interval(a(0), a(1))
}
