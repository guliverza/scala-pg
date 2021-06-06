package leetcode.october2020.week3

import org.scalatest.flatspec._
import org.scalatest.matchers._

import scala.io.Source

class RepeatedDNASequencesSpec extends AnyFlatSpec with should.Matchers {
  "RepeatedDNASequences" should "find repeated substrings" in {
    Source.fromResource("repeated.test").getLines.foreach { line =>
      println(RepeatedDNASequences.findRepeatedDnaSequences(line))
    }
  }
}
