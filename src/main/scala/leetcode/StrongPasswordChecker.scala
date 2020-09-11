package leetcode

object StrongPasswordChecker {
  val MaxLength = 20

  def strongPasswordChecker(s: String): Int = {
    val required = Seq(
      s.exists(c => c.isLetter && c.isLower),
      s.exists(c => c.isLetter && c.isUpper),
      s.exists(c => c.isDigit)).count(b => !b)
    val anyToAdd = if (s.length < 6) 6 - s.length else 0
    val extra = if (s.length > MaxLength) s.length - MaxLength else 0
    val oneLetterWords = split(s)
    val toRemove = if (s.length >= 3) oneLetterWords.map(word => word.length - 2).sum else 0
    val toReplace = if (s.length >= 3) oneLetterWords.map(word => word.length / 3).sum else 0
    val onlyOneLetterBulgeOut = oneLetterWords.exists(word => word.length % 3 == 0)

    val toRemoveOrSplit =
      (Some(math.abs(toReplace - anyToAdd)).filter(_ => toReplace + s.length <= MaxLength) ++
        Some(toReplace + extra) ++
        Some(toReplace + extra - 1).filter(_ => onlyOneLetterBulgeOut && extra > 0) ++
        Some(toRemove - extra)).min
    println(s"required=$required, anyToAdd=$anyToAdd, toRemove=$toRemove, toReplace=$toReplace, " +
      s"extra=$extra, toRemoveOrSplit=$toRemoveOrSplit, onlyOneLetterBulgeOut=$onlyOneLetterBulgeOut")

    Seq(required + extra, anyToAdd, extra, toRemoveOrSplit).max
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
