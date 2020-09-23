package leetcode.september2020.week4

import org.scalatest.flatspec._
import org.scalatest.matchers._

class GasStationSpec extends AnyFlatSpec with should.Matchers {
  "GasStation" should "check if clockwise circle travel is possible" in {
    GasStation.canCompleteCircuit(Array(1,2,3,4,5), Array(3,4,5,1,2)) should be (3)
    GasStation.canCompleteCircuit(Array(2,3,4), Array(3,4,3)) should be (-1)
  }

}
