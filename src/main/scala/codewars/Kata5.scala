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
          val result = x;
          x += 2;
          result
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

  case class Person(phone: String, name: String, address: String, duplicate: Boolean = false) {
    override def toString: String = {
      if (duplicate) s"Error => Too many people: $phone"
      else s"Phone => $phone, Name => $name, Address => $address"
    }
  }

  val phonePattern = """\+(\d\d?-\d\d\d-\d\d\d-\d\d\d\d)""".r
  val namePattern = """<([^>]+)>""".r

  def phone(file: String, num: String): String = {
    val dict = file.split("\n").foldLeft(Map.empty[String, Person]) {
      case (dict, line) =>
        (for {
          phoneMatch <- phonePattern.findFirstMatchIn(line)
          nameMatch <- namePattern.findFirstMatchIn(line)
          phone <- Option(phoneMatch.group(1))
          name <- Option(nameMatch.group(1))
        } yield {
          val address = line.replace(phone, "").replace(name, "").collect {
            case c if c.isLetterOrDigit || Set('-', '.').contains(c) => c
            case c if c.isWhitespace || c == '_' => ' '
          }.trim.replaceAll(" +", " ")
          Person(phone, name, address)
        }).fold(dict) { newPerson =>
          dict.get(newPerson.phone).fold(dict + (newPerson.phone -> newPerson)) { oldPerson =>
            dict + (newPerson.phone -> newPerson.copy(duplicate = true))
          }
        }
    }
    dict.get(num).map(_.toString).getOrElse(s"Error => Not found: $num")
  }

  val dr = "/+1-541-754-3010 156 Alphand_St. <J Steeve>\n 133, Green, Rd. <E Kustur> NY-56423 ;+1-541-914-3010\n" +
    "+1-541-984-3012 <P Reed> /PO Box 530; Pollocksville, NC-28573\n :+1-321-512-2222 <Paul Dive> Sequoia Alley PQ-67209\n" +
    "+1-741-984-3090 <Peter Reedgrave> _Chicago\n :+1-921-333-2222 <Anna Stevens> Haramburu_Street AA-67209\n" +
    "+1-111-544-8973 <Peter Pan> LA\n +1-921-512-2222 <Wilfrid Stevens> Wild Street AA-67209\n" +
    "<Peter Gone> LA ?+1-121-544-8974 \n <R Steell> Quora Street AB-47209 +1-481-512-2222\n" +
    "<Arthur Clarke> San Antonio $+1-121-504-8974 TT-45120\n <Ray Chandler> Teliman Pk. !+1-681-512-2222! AB-47209,\n" +
    "<Sophia Loren> +1-421-674-8974 Bern TP-46017\n <Peter O'Brien> High Street +1-908-512-2222; CC-47209\n" +
    "<Anastasia> +48-421-674-8974 Via Quirinal Roma\n <P Salinger> Main Street, +1-098-512-2222, Denver\n" +
    "<C Powel> *+19-421-674-8974 Chateau des Fosses Strasbourg F-68000\n <Bernard Deltheil> +1-498-512-2222; Mount Av.  Eldorado\n" +
    "+1-099-500-8000 <Peter Crush> Labrador Bd.\n +1-931-512-4855 <William Saurin> Bison Street CQ-23071\n" +
    "<P Salinge> Main Street, +1-098-512-2222, Denve\n"+ "<P Salinge> Main Street, +1-098-512-2222, Denve\n" +
    "/+5-541-754-3010 156 Alphandria_Street. <Jr Part>\n 1333, Green, Road <F Fulgur> NW-46423 ;+6-541-914-3010!\n" +
    "+5-541-984-3012 <Peter Reeves> /PO Box 5300; Albertville, SC-28573\n :+5-321-512-2222 <Paulo Divino> Boulder Alley ZQ-87209\n" +
    "+3-741-984-3090 <F Flanaghan> _Chicago Av.\n :+3-921-333-2222 <Roland Scorsini> Bellevue_Street DA-67209\n" +
    "+8-111-544-8973 <Laurence Pantow> SA\n +8-921-512-2222 <Raymond Stevenson> Joly Street EE-67209\n" +
    "<John Freeland> Mantow ?+2-121-544-8974 \n <Robert Mitch> Eleonore Street QB-87209 +2-481-512-2222?\n" +
    "<Arthur Paternos> San Antonio $+7-121-504-8974 TT-45121\n <Ray Charles> Stevenson Pk. !+7-681-512-2222! CB-47209,\n" +
    "<JP Gorce> +9-421-674-8974 New-Bern TP-16017\n <P McDon> Revolution Street +2-908-512-2222; PP-47209\n" +
    "<Elizabeth Corber> +8-421-674-8974 Via Papa Roma\n <C Saborn> Main Street, +15-098-512-2222, Boulder\n" +
    "<Colin Marshall> *+9-421-674-8974 Edinburgh UK\n <Bernard Povit> +3-498-512-2222; Hill Av.  Cameron\n" +
    "+12-099-500-8000 <Pete Highman> Ontario Bd.\n +8-931-512-4855 <W Mount> Oxford Street CQ-23071\n" +
    "<Donald Drinkaw> Moon Street, +3-098-512-2222, Peterville\n";
  def main(args: Array[String]): Unit = {
    println(phone(dr, "48-421-674-8974"))
    println(phone(dr, "1-098-512-2222"))
    println(phone(dr, "5-555-555-5555"))
    println(phone(dr, "1-541-754-3010"))
    println(gap(2, 3, 50))
    println(gap(4, 100, 110))
    val t = System.nanoTime
    //    println(gap(1000, 1, 1000000000))
    println(s"time ${(System.nanoTime - t) / 1000000}ms")
  }

}
