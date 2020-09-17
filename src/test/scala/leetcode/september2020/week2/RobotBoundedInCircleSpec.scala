package leetcode.september2020.week2

import leetcode.september2020.week2.BullsAndCows.getHint
import leetcode.september2020.week3.RobotBoundedInCircle
import org.scalatest.flatspec._
import org.scalatest.matchers._

class RobotBoundedInCircleSpec extends AnyFlatSpec with should.Matchers {

  "RobotBoundedInCircle" should "check if robot stays at home" in {
    RobotBoundedInCircle.isRobotBounded("GGLLGG") should be (true)
    RobotBoundedInCircle.isRobotBounded("LLGRL") should be (true)
  }

}
