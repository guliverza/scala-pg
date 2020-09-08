package leetcode

object StrongPasswordChecker {

  def strongPasswordChecker(s: String): Int = {
    val required = Seq(
      s.exists(c => c.isLetter && c.isLower),
      s.exists(c => c.isLetter && c.isUpper),
      s.exists(c => c.isDigit)).count(b => !b)
    val anyToAdd = if (s.length < 6) 6 - s.length else 0
    val extra = if (s.length > 20) s.length - 20 else 0
    val toRemove = if (s.length >= 3) split(s).map(word => word.length - 2).sum else 0
    val toSplit = if (s.length >= 3) split(s).map(word => word.length / 3).sum else 0
    val toRemoveOrSplit = if (toSplit > 0 && math.max(anyToAdd, anyToAdd) > 0)

    println(required, anyToAdd, toRemove, toSplit, extra)


    if (required == 0 || extra == 0)
      Seq(required, anyToAdd, toSplit, toRemove, extra).max
    else
      Seq(required + extra, anyToAdd, toSplit, toRemove, extra).max
  }

  def split(s: String): List[String] = {
    val (rep, word) = s.foldLeft((List.empty[String], "")) {
      case ((res, w), c) => if (w.isEmpty || w.contains(c)) (res, w :+ c)
      else if (w.length >= 3) (res :+ w, c.toString)
      else (res, c.toString)
    }
    if (word.length >= 3) rep :+ word else rep
  }

  def main(args: Array[String]): Unit = {
    //    println(split("aaa111"))
    println(strongPasswordChecker("1234567890123456Baaaaa"))
  }
}