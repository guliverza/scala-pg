package leetcode.september2020.week1

object WordPattern {

  def wordPatternSimple(pattern: String, str: String): Boolean = {
    val words = str.split(' ')
    (words.length == pattern.length) && {
      val (charsDistinct, wordsDistinct) = (pattern zip words).distinct.unzip
      (charsDistinct.length == charsDistinct.distinct.length) &&
        (wordsDistinct.length == wordsDistinct.distinct.length)
    }
  }

  def wordPattern(pattern: String, str: String): Boolean = {
    val words = str.split(" ")
    (pattern.length == words.length) && {
      val zipped = pattern.zip(words)
      val (_, _, res) = zipped.foldLeft((Map.empty[Char, String], Map.empty[String, Char], true)) {
        case ((char2word, word2char, res), (char, word)) =>
          val word1 = char2word.get(char)
          val char1 = word2char.get(word)
          if (word1.isEmpty && char1.isEmpty) {
            (char2word + (char -> word), word2char + (word -> char), res)
          } else {
            (char2word, word2char, res && word1.contains(word) && char1.contains(char))
          }
      }
      res
    }
  }

}
