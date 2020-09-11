package leetcode.september2020.week2

import jdk.internal.loader.Resource
import leetcode.september2020.week2.MaximumProductSubarray.maxProduct
import org.scalatest.flatspec._
import org.scalatest.matchers._

import scala.io.Source

class MaximumProductSubarraySpec extends AnyFlatSpec with should.Matchers {

  "Bulls and Cows" should "provide hints" in {
    maxProduct(Array(2, 3, -2, 4)) shouldBe 6
    maxProduct(Array(-2, 0, -1)) shouldBe 0
    maxProduct(Array(-3, -1, -1)) shouldBe 3
    maxProduct(Array(-2)) shouldBe -2
    maxProduct(Array(-1, 2, 4, 5)) shouldBe 40
    Source.fromResource("maxprod.test").getLines.foreach { line =>
      val split = line.split("=")
      val input = split(0).trim.split(",").map(_.trim.toInt).toArray
      val result = split(1).trim.toInt
      if (input.lengthCompare(35) > 0) {
        println(s"""${input.take(30).mkString("(", ",", "")}...${input.takeRight(3).mkString("",",",")")} => $result""")
      } else {
        println(s"""${input.mkString("(",",",")")} => $result""")
      }
      maxProduct(input) shouldBe result
    }
  }

}
