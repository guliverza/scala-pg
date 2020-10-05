package leetcode.october2020.week1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BitwiseComplementSpec extends AnyFlatSpec with should.Matchers {
  BitwiseComplement.bitwiseComplement(5) should be (2)
  BitwiseComplement.bitwiseComplement(7) should be (0)
  BitwiseComplement.bitwiseComplement(10) should be (5)
  BitwiseComplement.bitwiseComplement(16) should be (15)
  BitwiseComplement.bitwiseComplement(0) should be (1)
  BitwiseComplement.bitwiseComplement(1) should be (0)
}
