package example

object WordPattern {
  def wordPattern(pattern: String, str: String): Boolean = {
    val words = str.split(" ")
    if (pattern.length == words.length) {
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
    } else {
      false
    }
  }


  def main(args: Array[String]) = {
    println(wordPattern("abba", "dog dog dog dog"))
    println(wordPattern("abba", "dog cat cat dog"))
  }

}
