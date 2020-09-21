package leetcode.september2020.week3

object CarPooling {

  case class Trip(passengers: Int, start: Int, end: Int)

  def carPooling(_trips: Array[Array[Int]], capacity: Int): Boolean = {
    val trips = _trips.map(a => Trip(a(0), a(1), a(2))).sortBy(_.start)
    val (_, ok) = trips.foldLeft((List.empty[Trip], true)) { case ((passengers, ok), trip) =>
      val newPassengers = passengers.filter(_.end > trip.start) :+ trip
      val cargo = newPassengers.map(_.passengers).sum
      (newPassengers, ok && cargo <= capacity)
    }
    ok
  }

  def main(args: Array[String]): Unit = {
    println(carPooling(Array(Array(2, 1, 5), Array(3, 3, 7)), capacity = 4))
    println(carPooling(Array(Array(2, 1, 5), Array(3, 3, 7)), capacity = 5))
    println(carPooling(Array(Array(2, 1, 5), Array(3, 5, 7)), capacity = 3))
    println(carPooling(Array(Array(3, 2, 7), Array(3, 7, 9), Array(8, 3, 9)), capacity = 11))
  }
}
