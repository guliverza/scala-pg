package leetcode.october2020.week1

class RecentCounter {
  var pings: List[Int] = Nil
  def ping(t: Int): Int = {
    pings = t :: pings
    pings.takeWhile(_ >= t - 3000).size
  }
}
