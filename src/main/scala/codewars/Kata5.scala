package codewars

import scala.math.BigInt

object Kata5 {

  def factors(m: Int): String = {
    val sqrt = math.sqrt(m).toInt
    val (rest, divs) = pseudoPrimes.takeWhile(_ <= sqrt).foldLeft((m, List.empty[(Int, Int)])) {
      case ((m, list), d) =>
        val (newM, k) = factor(m, d)
        if (k > 0) (newM, list :+ (d, k))
        else (m, list)

    }
    val withRest = if (rest > 1) divs :+ (rest, 1) else divs
    withRest.map { case (n, k) => if (k == 1) s"($n)" else s"($n**$k)" }.mkString("")
  }

  lazy val pseudoPrimes: LazyList[Int] = 2 #:: LazyList.from(3, 2)
  //  lazy val primes: LazyList[Int] = 2 #:: LazyList.from(3, 2).filter(n =>
  //    primes.takeWhile { j => j * j <= n }.forall { k => n % k > 0 });

  def factor(n: Int, k: Int): (Int, Int) = {
    if (n % k != 0) {
      (n, 0)
    } else {
      val (newM, x) = factor(n / k, k)
      (newM, x + 1)
    }
  }

  def mixbonacci(pattern: List[String], length: Int): List[BigInt] = {
    val mix = Map(
      "fib" -> new Fibo,
      "pad" -> new Padovan,
      "jac" -> new Jacobsthal,
      "pel" -> new Pell,
      "tri" -> new Tribonacci,
      "tet" -> new Tetranacci)

    val iter = new CyclicIterator[String, List[String]](pattern)
    (1 to length).zip(iter).map { case (_, pat) => mix(pat).next }.toList
  }

  class Fibo extends RecursiveSeq(Seq(0, 1)) {
    override def formula(prev: Seq[BigInt]): BigInt = prev.sum
  }

  class Padovan extends RecursiveSeq(Seq(1, 0, 0)) {
    override def formula(a: Seq[BigInt]): BigInt = a(1) + a(0)
  }

  class Jacobsthal extends RecursiveSeq(Seq(0, 1)) {
    override def formula(a: Seq[BigInt]): BigInt = a(1) + 2 * a(0)
  }

  class Pell extends RecursiveSeq(Seq(0, 1)) {
    override def formula(a: Seq[BigInt]): BigInt = 2 * a(1) + a(0)
  }

  class Tribonacci extends RecursiveSeq(Seq(0, 0, 1)) {
    override def formula(a: Seq[BigInt]): BigInt = a.sum
  }

  class Tetranacci extends RecursiveSeq(Seq(0, 0, 0, 1)) {
    override def formula(a: Seq[BigInt]): BigInt = a.sum
  }

  abstract class RecursiveSeq(var start: Seq[BigInt]) extends Iterator[BigInt] {
    var work: Seq[BigInt] = Nil

    def next: BigInt = {
      if (start.nonEmpty) {
        val element = start.head
        start = start.tail
        work = work :+ element
        element
      } else {
        val next = formula(work)
        work = work.tail :+ next
        next
      }
    }

    override def hasNext: Boolean = true

    def formula(prev: Seq[BigInt]): BigInt
  }

  class CyclicIterator[E, C <: Seq[E]](val mElements: C) extends Iterator[E] {
    private var mIterator: Iterator[E] = mElements.iterator

    override def hasNext: Boolean = {
      if (!mIterator.hasNext) mIterator = mElements.iterator
      mIterator.hasNext
    }

    override def next: E = {
      if (!mIterator.hasNext) mIterator = mElements.iterator
      mIterator.next
    }
  }

}
