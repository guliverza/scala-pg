package leetcode.september2020.week4

import scala.collection.mutable
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
    } else if (m.entryExists(a, d => d.a == a && d.b == b || d.b == a && d.a == b)) {
      val existing = m(a).find(d => d.a == a && d.b == b || d.b == a && d.a == b).get
      if (existing.a == a) existing.value
      else 1.0 / existing.value
    } else {
      (for {
        eqWithA <- m.getOrElse(a, Set.empty)
        eqWithB <- m.getOrElse(b, Set.empty)
      } yield {
        if (eqWithA.a == a && eqWithB.a == b) eqWithA.value / eqWithB.value * calc(eqWithA.b, eqWithB.b, m)
        else if (eqWithA.b == a && eqWithB.b == b) eqWithB.value / eqWithA.value * calc(eqWithA.a, eqWithB.a, m)
        else if (eqWithA.a == a && eqWithB.b == b) eqWithA.value * eqWithB.value * calc(eqWithA.b, eqWithB.a, m)
        else 1.0 / (eqWithA.value * eqWithB.value) * calc(eqWithA.a, eqWithB.b, m)
      }).find(_ != -1.0d).getOrElse(-1.0d)
    }
  }

  def main(args: Array[String]): Unit = {
    println(12.0 / 2 * 3)
  }

}
