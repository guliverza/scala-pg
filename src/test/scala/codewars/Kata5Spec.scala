package codewars

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Kata5Spec extends AnyFlatSpec with should.Matchers {
  "mixbonacci" should "calculate mixbonacci" in {
    Kata5.mixbonacci(List[String]("fib"), 0) should be(List[BigInt]())
    Kata5.mixbonacci(List[String]("fib"), 10) should be(List[BigInt](0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
    Kata5.mixbonacci(List[String]("pad"), 10) should be(List[BigInt](1, 0, 0, 1, 0, 1, 1, 1, 2, 2))
    Kata5.mixbonacci(List[String]("jac"), 10) should be(List[BigInt](0, 1, 1, 3, 5, 11, 21, 43, 85, 171))
    Kata5.mixbonacci(List[String]("pel"), 10) should be(List[BigInt](0, 1, 2, 5, 12, 29, 70, 169, 408, 985))
    Kata5.mixbonacci(List[String]("tri"), 10) should be(List[BigInt](0, 0, 1, 1, 2, 4, 7, 13, 24, 44))
    Kata5.mixbonacci(List[String]("tet"), 10) should be(List[BigInt](0, 0, 0, 1, 1, 2, 4, 8, 15, 29))
    Kata5.mixbonacci(List[String]("fib", "tet"), 10) should be(List[BigInt](0, 0, 1, 0, 1, 0, 2, 1, 3, 1))
    Kata5.mixbonacci(List[String]("jac", "jac", "pel"), 10) should be(List[BigInt](0, 1, 0, 1, 3, 1, 5, 11, 2, 21))
  }

}
