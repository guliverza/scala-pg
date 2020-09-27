package leetcode.september2020.week4

import scala.collection.{immutable, mutable}
import scala.collection.mutable.{HashMap, MultiMap, Set}

object EvaluateDivision {

  case class Division(a: String, b: String, value: Double) {
    def plus(o: Division): Division = if (o.isZero) this
    else if (this.isZero) o
    else if (a == o.b) Division(o.a, b, value * o.value)
    else if (b == o.a) Division(a, o.b, value * o.value)
    else if (a == o.a) Division(o.b, b, value / o.value)
    else if (b == o.b) Division(a, o.a, value / o.value)
    else throw new IllegalStateException(s"Unreachable link $this + $o")

    def isZero: Boolean = a == "*" && b == "*"

    override def toString: String = s"$a/$b=$value"
  }

  object Division {
    val Zero = Division("*", "*", 1.0)
  }


  def calcEquation(equations: List[List[String]], values: Array[Double], queries: List[List[String]]): Array[Double] = {
    val m = new HashMap[String, Set[Division]] with MultiMap[String, Division]
    equations.zip(values).foreach {
      case (a, v) =>
        val division = Division(a.head, a(1), v)
        m.addBinding(division.a, division)
        m.addBinding(division.b, division)
    }
    queries.map { query =>
      calc(query.head, query(1), m)
    }.toArray
  }

  def calc(a: String, b: String, m: mutable.MultiMap[String, Division]): Double = {
    if (a == b && m.keySet.contains(a) && m.keySet.contains(b)) {
      1.0
    } else if (m.entryExists(a, d => d.a == a && d.b == b)) {
      m(a).find(d => d.a == a && d.b == b).get.value
    } else if (m.entryExists(b, d => d.a == b && d.b == a)) {
      1.0 / m(b).find(d => d.a == b && d.b == a).get.value
    } else {
      (for {
        x <- m.getOrElse(a, mutable.Set.empty).to(LazyList)
        y <- m.getOrElse(b, mutable.Set.empty).to(LazyList)
        value <- calcDivision(x, y, m, a, b)
      } yield {
        println(s"$a/$b => $value")
        value
      }).headOption.getOrElse(-1.0)
    }
  }

  def wayBack(to: Division, m: mutable.MultiMap[String, Division], path: Map[Division, Int]): Division = {
    println(s"to=$to")
    val (res, _) = ((path(to)-1) to 0 by -1).foldLeft((to, to)) { case ((res, prev), t) =>
      val transitions = (m.get(prev.a) ++ m.get(prev.b)).flatten.toSet
      val transition = transitions.find { x => path(x) == t }
      (res.plus(transition.get), transition.get)
    }
    println(res)
    res
  }

  def calcDivision(from: Division, to: Division, m: mutable.MultiMap[String, Division], a: String, b: String): Option[Double] = {
    val visited = m.values.flatten.map(_ -> -1).toMap + (from -> 0)
    val path = findPath(m, visited, 0, to)
    if (path(to) != -1) {
      val res = wayBack(to, m, path)
      if (res.a == a && res.b == b) Some(res.value)
      else if (res.a == b && res.b == a) Some(1.0 / res.value)
      else None
    } else {
      None
    }
  }

  def findPath(m: mutable.MultiMap[String, Division], a: Map[Division, Int], t: Int, destination: Division): Map[Division, Int] = {
    val lastStep = a.collect { case (d, tt) if tt == t => d }
    val newA = lastStep.foldLeft(a) { case (a, v) =>
      val transitions = (m.get(v.a) ++ m.get(v.b)).flatten.toSet
      transitions.foldLeft(a) { case (a, x) => if (a(x) == -1) a + (x -> (t + 1)) else a }
    }
    if (lastStep.nonEmpty && newA(destination) == -1) {
      findPath(m, newA, t + 1, destination)
    } else {
      newA
    }
  }

  def main(args: Array[String]): Unit = {
    println(10 to 1 by -1)
  }

}
