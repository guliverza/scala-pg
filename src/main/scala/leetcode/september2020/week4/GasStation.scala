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
    if (gas.sum >= cost.sum) {
      val (start, _) = (0 until gas.length).foldLeft((0, 0)) { case ((start, tank), n) =>
        val newTank = tank + gas(n) - cost(n)
        if (newTank >= 0) {
          (start, newTank)
        } else {
          (n+1, 0)
        }
      }
      start
    } else {
      -1
    }
  }


}
