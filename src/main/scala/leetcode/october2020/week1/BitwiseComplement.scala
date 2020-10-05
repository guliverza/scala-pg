package leetcode.october2020.week1

object BitwiseComplement {
  def bitwiseComplement(n: Int): Int = {
    if (n == 0) {
      1
    } else {
      val zeroBits = 32 - highestBit(n)
//      println(s"n = ${n.toBinaryString}, bit = $zeroBits, ~n = ${(~n).toBinaryString}, " +
//        s"~n << bit = ${(~n << zeroBits).toBinaryString}, " +
//        s"~n << bit >>> bit = ${(~n << zeroBits >>> zeroBits).toBinaryString}")
      ~n << zeroBits >>> zeroBits
    }
  }

  def highestBit(number: Int): Int = {
    val bit = (0 to 31).find(bit => (1 << bit) > number)
    bit.getOrElse(31)
  }
}
