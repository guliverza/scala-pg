package leetcode.october2020.week1

import org.scalatest.flatspec._
import org.scalatest.matchers._

class RecentCounterSpec extends AnyFlatSpec with should.Matchers {
  "RecentCounter" should "count recent calls" in {
    val counter = new RecentCounter()
    counter.ping(1) should be (1)
    counter.ping(100) should be (2)
    counter.ping(3001) should be (3)
    counter.ping(3002) should be (3)
  }

}
