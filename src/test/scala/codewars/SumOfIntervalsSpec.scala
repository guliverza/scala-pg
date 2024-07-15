package codewars

import org.scalatest._
import flatspec._

class SumOfIntervalsSpec extends AnyFlatSpec {
  "sumOfIntervals" should "pass basic tests" in {
    val testCases = List( //intervals, expected
      (List((1, 5)), 4),
      (List((1, 5), (6, 10)), 8),
      (List((1, 5), (1, 5)), 4),
      (List((1, 4), (7, 10), (3, 5)), 7),
      (List((1, 10), (7, 10), (3, 5)), 9),
    )

    testCases.foreach {
      case (intervals, expected) => assertResult(expected, s"\nInput\n  intervals = $intervals") {SumOfIntervals.sumOfIntervals(intervals)}
    }
  }

  it should "pass larger tests" in {
    val testCases = List( //intervals, expected
      (List((-1000000000, 1000000000)),  2000000000),
      (List((0, 20), (-1000000000, 10), (30, 40)),  1000000030),
    )

    testCases.foreach {
      case (intervals, expected) => assertResult(expected, s"\nInput\n  intervals = $intervals") {SumOfIntervals.sumOfIntervals(intervals)}
    }
  }
}