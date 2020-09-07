package leetcode

object StrongPasswordChecker {
  /**A password is considered strong if below conditions are all met:

    It has at least 6 characters and at most 20 characters.
    It must contain at least one lowercase letter, at least one uppercase letter, and at least one digit.
    It must NOT contain three repeating characters in a row
      ("...aaa..." is weak, but "...aa...a..." is strong, assuming other conditions are met).
    Write a function strongPasswordChecker(s), that takes a string s as input,
      and return the MINIMUM change required to make s a strong password.
      If s is already strong, return 0.

    Insertion, deletion or replace of any one character are all considered as one change.
   *
   */
  def strongPasswordChecker(s: String): Int = {
    val required = Seq(
      s.exists(c => c.isLetter && c.isLower),
      s.exists(c => c.isLetter && c.isUpper),
      s.exists(c => c.isDigit)).count(b => !b)
    val anyToAdd = if (s.length < 6) 6 - s.length else 0
    val extra = if (s.length > 20) s.length - 20 else 0
    val toSplit = if (s.length >= 3) split(s).map(word => word.length/3).sum else 0
    println(required, anyToAdd, toSplit, extra)
    Seq(required, anyToAdd, toSplit, extra).max
  }

  def split(s: String): List[String] = {
    val (rep, word) = s.foldLeft((List.empty[String], "")) {
      case ((res, w), c) => if (w.isEmpty || w.contains(c)) (res, w :+ c)
      else if (w.length >= 3) (res :+ w, c.toString)
      else (res, c.toString)
    }
    if (word.length >= 3) rep :+ word else rep
  }

}
