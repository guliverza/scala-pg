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

  "PrimeDecomp" should "decompose to primes" in {
    Kata5.pseudoPrimes.take(10).toList should be (List(2, 3, 5, 7, 9, 11, 13, 15, 17, 19))

    time(Kata5.factors(7775460) should be("(2**2)(3**3)(5)(7)(11**2)(17)"))
    time(Kata5.factors(7919) should be("(7919)"))
    time(Kata5.factors(933555431) should be("(7537)(123863)"))
    time(Kata5.factors(342217392) should be("(2**4)(3)(11)(43)(15073)"))
  }

  def time[T](f: => T): T = {
    val start = System.nanoTime()
    val ret = f
    val end = System.nanoTime()
    println(s"Time taken: ${(end - start) / 1000 / 1000} ms")
    ret
  }
}
