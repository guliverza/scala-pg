package codewars

import scala.math.BigInt

object Kata5 {

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
