package common

case class Interval(from: Int, to: Int) {
  def before(o: Interval): Boolean = to < o.from

  def after(o: Interval): Boolean = from > o.to

  def intersects(o: Interval): Boolean = !before(o) && !after(o)

  def intersection(o: Interval): Interval = Interval(math.max(from, o.from), math.min(to, o.to))

  def union(o: Interval): Interval = Interval(math.min(from, o.from), math.max(to, o.to))

  def coveredBy(o: Interval): Boolean = from >= o.from && to <= o.to

  def asArray: Array[Int] = Array(from, to)

  def length: Int = to - from

  override def toString: String = s"($from, $to)"
}

object Interval {
  def apply(a: Array[Int]): Interval = Interval(a(0), a(1))
  def apply(tuple: (Int, Int)): Interval = Interval(tuple._1, tuple._2)
}
