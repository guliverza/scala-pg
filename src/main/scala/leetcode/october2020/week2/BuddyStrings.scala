package leetcode.october2020.week2

object BuddyStrings {
  def buddyStrings(a: String, b: String): Boolean = {
    a.length == b.length && {
      a.zip(b).filter { case (a, b) => a != b } match {
        case IndexedSeq() => a.toSet.size < a.length
        case IndexedSeq(x, y) => x.swap == y
        case _ => false
      }
    }
  }

}
