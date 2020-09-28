package leetcode.september2020.week4

import org.scalatest.flatspec._
import org.scalatest.matchers._

class EvaluateDivisionSpec extends AnyFlatSpec with should.Matchers {
  "EvaluateDivision" should "evaluate division formulas" in {
    EvaluateDivision.calcEquation(
      equations = List(List("a","b"),List("b","c")),
      values = Array(2.0,3.0),
      queries = List(List("a","c"),List("b","a"),List("a","e"),List("a","a"),List("x","x"))
    ) should be (Array(6.0, 0.5, -1.0, 1.0, -1.0))

    EvaluateDivision.calcEquation(
      equations = List(List("a","b"),List("b","c"),List("bc","cd")),
      values = Array(1.5,2.5,5.0),
      queries = List(List("a","c"),List("c","b"),List("bc","cd"),List("cd","bc"))
    ) should be (Array(3.75000,0.40000,5.00000,0.20000))

    EvaluateDivision.calcEquation(
      equations = List(List("x1","x2"),List("x2","x3"),List("x3","x4"),List("x4","x5")),
      values = Array(3.0,4.0,5.0,6.0),
      queries = List(List("x1","x5"),List("x5","x2"),List("x2","x4"),List("x2","x2"),List("x2","x9"),List("x9","x9")),
    ) should be (Array(360.0, 0.008333333333333333, 20.0, 1.0, -1.0, -1.0))
  }

}
