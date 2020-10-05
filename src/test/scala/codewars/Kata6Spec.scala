package codewars

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Kata6Spec extends AnyFlatSpec with should.Matchers {
  "stockSummary" should "count books" in {
    Kata6.stockSummary(
      Array("BBAR 150", "CDXE 515", "BKWR 250", "BTSQ 890", "DRTY 600"),
      Array("A", "B", "C", "D")
    ) should be ("(A : 0) - (B : 1290) - (C : 515) - (D : 600)")
    Kata6.stockSummary(
      Array("ABAR 200", "CDXE 500", "BKWR 250", "BTSQ 890", "DRTY 600"),
      Array("A", "B")
    ) should be ("(A : 200) - (B : 1140)")
  }

}
