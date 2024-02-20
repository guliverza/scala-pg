package leetcode

import scala.util.matching.Regex

object ValidNumber {
  def isNumber(s: String): Boolean = {
    val numberPattern: Regex = "^[+-]?[0-9]+[.]?[0-9]*([eE][+-]?[0-9]+)?$".r
    val numberPattern2: Regex = "^[+-]?[0-9]*[.]?[0-9]+([eE][+-]?[0-9]+)?$".r

    numberPattern.findFirstMatchIn(s).nonEmpty || numberPattern2.findFirstMatchIn(s).nonEmpty
  }
}
