package leetcode.september2020.week2

object BullsAndCows {
  /**
   * You are playing the following Bulls and Cows game with your friend:
   * You write down a number and ask your friend to guess what the number is.
   * Each time your friend makes a guess, you provide a hint that indicates
   * how many digits in said guess match your secret number exactly in both digit and position (called "bulls") and
   * how many digits match the secret number but locate in the wrong position (called "cows").
   * Your friend will use successive guesses and hints to eventually derive the secret number.
   *
   * Write a function to return a hint according to the secret number and friend's guess,
   * use A to indicate the bulls and B to indicate the cows.
   *
   * Please note that both secret number and friend's guess may contain duplicate digits.
   */

  def getHint(secret: String, guess: String): String = {
    val bulls = secret.zip(guess).count { case (s, g) => s == g }
    val cows = secret.toSeq.intersect(guess).length - bulls
    s"${bulls}A${cows}B"
  }

}
