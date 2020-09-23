package leetcode.september2020.week4

import scala.annotation.tailrec

object GasStation {
  /**
   * There are N gas stations along a circular route, where the amount of gas at station i is gas[i].
   *
   * You have a car with an unlimited gas tank and it costs cost[i] of gas to travel from station i to its next station (i+1).
   * You begin the journey with an empty tank at one of the gas stations.
   *
   * Return the starting gas station's index if you can travel around the circuit once in the clockwise direction, otherwise return -1.
   *
   * Note:
   *
   * If there exists a solution, it is guaranteed to be unique.
   * Both input arrays are non-empty and have the same length.
   * Each element in the input arrays is a non-negative integer.
   */
  def canCompleteCircuit(gas: Array[Int], cost: Array[Int]): Int = {
    val count = gas.length

    @tailrec
    def travel(start: Int, current: Int, tank: Int): Int = {
      val next = (current + 1) % count
      if (tank + gas(current) >= cost(current)) {
        if (next == start) {
          start
        } else {
          travel(start, next, tank + gas(current) - cost(current))
        }
      } else {
        -1
      }
    }

    if (gas.sum >= cost.sum) {
      (0 until count).find { i =>
        travel(i, i, 0) != -1
      }.getOrElse(-1)
    } else {
      -1
    }
  }


}
