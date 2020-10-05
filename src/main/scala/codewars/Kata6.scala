package codewars

object Kata6 {
  def stockSummary(lstOfArt: Array[String], lstOfCat: Array[String]): String = {
    if (lstOfArt.isEmpty || lstOfCat.isEmpty) {
      ""
    } else {
      lstOfArt.foldLeft(lstOfCat.map(cat => (cat, 0)).toMap) {
        case (sum, book) =>
          val split = book.split(" ")
          val cat = split(0).take(1)
          val count = split(1).toInt
          sum.get(cat).fold(sum) { existing =>
            sum + (cat -> (count + existing))
          }
      }.toList
        .sortBy { case (cat, count) => lstOfCat.indexOf(cat) }
        .map { case (cat, count) => s"($cat : $count)" }
        .mkString(" - ")
    }
  }

}