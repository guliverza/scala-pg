package codewars

import org.scalatest.Assertions._
import SqInRectTest._
import org.scalatest.flatspec.AnyFlatSpec

class SqInRectTest extends AnyFlatSpec {
  it should "pass basic tests" in {
    testing(5, 3, Array(3, 2, 1, 1))
    testing(3, 5, Array(3, 2, 1, 1))
    testing(5, 5, Array())

  }
}

object SqInRectTest {

  private def testing(min: Int, max: Int, expect: Array[Int]): Unit = {
    println("MIN: " + min + " MAX: " + max)
    val actual: Array[Int] = SqInRect.sqInRect(min, max)
    println("Actual: " + actual.mkString(", "))
    println("Expect: " + expect.mkString(", "))
    println("-")
    assertResult(expect){actual}
  }
}