package leetcode.september2020.week2

object CompareVersion {
  /**
   * Compare two version numbers version1 and version2.
   * If version1 > version2 return 1; if version1 < version2 return -1;otherwise return 0.
   *
   * You may assume that the version strings are non-empty and contain only digits and the . character.
   *
   * The . character does not represent a decimal point and is used to separate number sequences.
   *
   * For instance, 2.5 is not "two and a half" or "half way to version three",
   * it is the fifth second-level revision of the second first-level revision.
   *
   * You may assume the default revision number for each level of a version number to be 0.
   * For example, version number 3.4 has a revision number of 3 and 4 for its first and second level revision number.
   * Its third and fourth level revision number are both 0.
   */
  def compareVersion(version1: String, version2: String): Int = {
    val v1 = version1.split("\\.").toList.map(_.toInt)
    val v2 = version2.split("\\.").toList.map(_.toInt)
    (v1 zip v2).map {
      case (v1, v2) => v1.compareTo(v2)
    }.find(_ != 0).getOrElse {
      val rest = if (v1.lengthCompare(v2) > 0) v1.slice(v2.length, v1.length)
      else v2.slice(v1.length, v2.length)
      if (rest.exists(_ != 0)) v1.lengthCompare(v2)
      else 0
    }
  }
}
