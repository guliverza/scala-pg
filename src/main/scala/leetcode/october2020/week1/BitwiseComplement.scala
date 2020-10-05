package leetcode.october2020.week1

object BitwiseComplement {
  def bitwiseComplement(n: Int): Int = {
    if (n == 0) {
      1
    } else {
      val zeroBits = 32 - highestBit(n)
      ~n << zeroBits >>> zeroBits
    }
  }

  def highestBit(number: Int): Int = {
    val bit = (0 to 31).find(bit => (1 << bit) > number)
    bit.getOrElse(31)
  }
}
