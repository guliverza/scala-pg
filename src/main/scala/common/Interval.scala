package common

case class Interval(from: Int, to: Int) {
  def before(o: Interval): Boolean = to < o.from

  def after(o: Interval): Boolean = from > o.to

  def intersects(o: Interval): Boolean = !before(o) && !after(o)

  def intersection(o: Interval): Interval = Interval(math.max(from, o.from), math.min(to, o.to))

  def coveredBy(o: Interval): Boolean = from >= o.from && to <= o.to

  def asArray: Array[Int] = Array(from, to)
}

object Interval {
  def apply(a: Array[Int]): Interval = Interval(a(0), a(1))
}
