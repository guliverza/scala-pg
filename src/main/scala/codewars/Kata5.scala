package codewars

import scala.collection.{BitSet, immutable, mutable}
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

  val TimeUnits = Seq((365 * 24 * 3600, "year"), (24 * 3600, "day"), (3600, "hour"), (60, "minute"), (1, "second"))

  def formatDuration(seconds: Int): String = {
    val (_, res) = TimeUnits.foldLeft((seconds, List.empty[(Int, String)])) {
      case ((secondsLeft, res), (unitSize, unitName)) =>
        (secondsLeft % unitSize, res :+ (secondsLeft / unitSize, unitName))
    }
    res.collect {
      case (1, unit) => s"1 $unit"
      case (count, unit) if count > 1 => s"$count ${unit}s"
    } match {
      case Nil => "now"
      case head :: Nil => head
      case list => list.init.mkString(", ") + " and " + list.last
    }
  }

  def isPrime(end: Long): Boolean = {
    val intSqrt = Math.sqrt(end).toInt
    val lazyInts = 2 #:: LazyList.from(3, 2)
    !lazyInts.takeWhile(_ <= intSqrt).exists(n => end % n == 0)
  }

  def gap(g: Int, m: Long, n: Long): String = {
    println(g, m, n)
    val primes = LazyList.from(new IterableOnce[Long] {
      override def iterator: Iterator[Long] = new Iterator[Long] {
        var x = m + 1 - m % 2

        override def hasNext: Boolean = x <= n

        override def next(): Long = {
          val result = x; x += 2; result
        }
      }
    }).filter(isPrime)
    if (primes.isEmpty) ""
    else primes.zip(primes.tail).find { case (p1, p2) => p2 - p1 == g }.map(_.toString()).getOrElse("")
  }


  def gap1(g: Int, m: Long, n: Long): String = {
    var x = m + 1 - m % 2
    var prevPrime: Long = -g
    while (x <= n) {
      if (isPrime(x)) {
        if (x - prevPrime == g) {
          return (prevPrime, x).toString
        }
        prevPrime = x
      }
      x += 2
    }
    ""
  }

  def main(args: Array[String]): Unit = {
    println(gap(2, 3, 50))
    println(gap(4, 100, 110))
    val t = System.nanoTime
    //    println(gap(1000, 1, 1000000000))
    println(s"time ${(System.nanoTime - t) / 1000000}ms")
  }

}
