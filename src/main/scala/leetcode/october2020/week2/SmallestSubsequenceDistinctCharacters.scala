package leetcode.october2020.week2

object SmallestSubsequenceDistinctCharacters {
  def smallestSubsequence(s: String): String = {
    s.foldLeft(("", s.toSeq.groupBy(c => c).view.mapValues(_.length).toMap)) {
      case ((res, counts), c) if !res.contains(c) =>
        val rollback = res.reverse.takeWhile(x => x > c && counts(x) > 0)
        (res.dropRight(rollback.length) + c, counts + (c -> (counts(c) - 1)))
      case ((res, counts), c) =>
        (res, counts + (c -> (counts(c) - 1)))
    }._1
  }
}
