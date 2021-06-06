package leetcode.october2020.week3

import scala.collection.IterableFactory.toBuildFrom

object RepeatedDNASequences {
  def findRepeatedDnaSequences(s: String): List[String] = {
    val l: LazyList[Char] = LazyList(s.toSeq:_*)
    l.sliding(10, 1).map(_.mkString("")).filter(substr => substr.length == 10 && s.indexOf(substr) != s.lastIndexOf(substr)).toList.distinct
  }

  def main(a: Array[String]): Unit = {
    println(findRepeatedDnaSequences("AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT"))
  }
}
