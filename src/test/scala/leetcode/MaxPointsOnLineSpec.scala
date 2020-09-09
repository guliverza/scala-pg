package leetcode

import leetcode.MaxPointsOnLine.maxPoints
import org.scalatest._
import flatspec._
import matchers._
class MaxPointsOnLineSpec extends AnyFlatSpec with should.Matchers {
  "MaxPointsOnLine" must "find lines" in {
    maxPoints(Array(Array(1, 1), Array(1, 1), Array(2, 2), Array(2, 2))) should be(4)
    maxPoints(Array(Array(0, 0), Array(1, 1), Array(1, -1))) should be(2)
    maxPoints(Array(Array(1, 1), Array(2, 2), Array(3, 3))) should be(3)
    maxPoints(Array(Array(2, 3), Array(3, 3), Array(-5, 3))) should be(3)
    maxPoints(Array(Array(1, 1), Array(2, 2))) should be(2)
    maxPoints(Array(Array(1, 1))) should be(1)
    maxPoints(Array(Array(0, 0), Array(0, 0))) should be(2)
    maxPoints(Array(Array(0,0),Array(94911150,94911151),Array(94911151,94911152))) should be(2)
    maxPoints(Array()) should be(0)
    maxPoints(Array(Array(15,12),Array(9,10),Array(-16,3),Array(-15,15),Array(11,-10),Array(-5,20),Array(-3,-15),Array(-11,-8),Array(-8,-3),Array(3,6),Array(15,-14),Array(-16,-18),Array(-6,-8),Array(14,9),Array(-1,-7),Array(-1,-2),Array(3,11),Array(6,20),Array(10,-7),Array(0,14),Array(19,-18),Array(-10,-15),Array(-17,-1),Array(8,7),Array(20,-18),Array(-4,-9),Array(-9,16),Array(10,14),Array(-14,-15),Array(-2,-10),Array(-18,9),Array(7,-5),Array(-12,11),Array(-17,-6),Array(5,-17),Array(-2,-20),Array(15,-2),Array(-5,-16),Array(1,-20),Array(19,-12),Array(-14,-1),Array(18,10),Array(1,-20),Array(-15,19),Array(-18,13),Array(13,-3),Array(-16,-17),Array(1,0),Array(20,-18),Array(7,19),Array(1,-6),Array(-7,-11),Array(7,1),Array(-15,12),Array(-1,7),Array(-3,-13),Array(-11,2),Array(-17,-5),Array(-12,-14),Array(15,-3),Array(15,-11),Array(7,3),Array(19,7),Array(-15,19),Array(10,-14),Array(-14,5),Array(0,-1),Array(-12,-4),Array(4,18),Array(7,-3),Array(-5,-3),Array(1,-11),Array(1,-1),Array(2,16),Array(6,-6),Array(-17,9),Array(14,3),Array(-13,8),Array(-9,14),Array(-5,-1),Array(-18,-17),Array(9,-10),Array(19,19),Array(16,7),Array(3,7),Array(-18,-12),Array(-11,12),Array(-15,20),Array(-3,4),Array(-18,1),Array(13,17),Array(-16,-15),Array(-9,-9),Array(15,8),Array(19,-9),Array(9,-17))) should be(6)
  }
}
