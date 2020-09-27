package leetcode.september2020.week4

import scala.collection.{immutable, mutable}
import scala.collection.mutable.{HashMap, MultiMap, Set}

object EvaluateDivision {

  case class Division(a: String, b: String, value: Double) {
    def plus(o: Division): Division = if (a == o.b)
      Division(o.a, b, value * o.value)
    else if (b == o.a)
      Division(a, o.b, value * o.value)
    else if (a == o.a)
      Division(o.b, b, value / o.value)
    else if (b == o.b)
      Division(a, o.a, value / o.value)
    else
      throw new IllegalStateException(s"Unreachable link $this + $o")

    override def toString: String = s"$a/$b=$value"
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
        value <- calcDivision(x, y, m)
      } yield {
        value
      }).headOption.getOrElse(-1.0)
    }
  }

  def wayBack(to: Division, m: mutable.MultiMap[String, Division], path: Map[Division, Int]): Double = {
    println("---")
    path.foreach(println)
    val maxT = path(to)
    println(s"to=$to")
    val way = (maxT to 0 by -1).map { t =>
      val transition = path.find { case (d, tt) => tt == t }
      println(s"transition=$transition")
      transition.get._1
    }.reverse
    way.tail.fold(way.head)(_.plus(_)).value
  }

  def calcDivision(from: Division, to: Division, m: mutable.MultiMap[String, Division]): Option[Double] = {
    val visited = m.values.flatten.map(_ -> -1).toMap + (from -> 0)
    val path = findPath(m, visited, 0, to)
    if (path(to) != -1) {
      Some(wayBack(to, m, path))
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
