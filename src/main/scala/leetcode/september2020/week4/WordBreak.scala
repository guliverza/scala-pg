package leetcode.september2020.week4

object WordBreak {
  def wordBreak(s: String, wordDict: List[String]): Boolean = {
    val bulk = wordDict.mkString("")
    s.forall(letter => bulk.contains(letter)) && check(s, reduceWords(wordDict).sortBy(_.length*(-1)))
  }

  def check(s: String, wordDict: List[String]): Boolean = {
    wordDict.filter(word => s.startsWith(word)).exists(word => {
      val n = LazyList.from(1).find(n => !s.startsWith(word * n)).map(_ - 1).getOrElse(1)
      val newS = s.drop(word.length * n)
      newS.isEmpty ||
        check(newS, wordDict) ||
        (n >= 2 && check(word + newS, wordDict)) ||
        (n >= 3 && check((word * 2) + newS, wordDict))
    })
  }

  def reduceWords(words: List[String]): List[String] = {
    val filtered = words.filterNot(www =>
      words.exists(w => w.length < www.length && (w * (www.length / w.length) == www))
    )
    filtered.filterNot(www =>
      filtered.exists(w => www.startsWith(w) && filtered.contains(www.drop(w.length)))
    )
  }
}
