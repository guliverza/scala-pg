package leetcode.september2020.week3

object MaximumXOR {

  def findMaximumXOR(nums: Array[Int]): Int = {
    val high = highestBit(nums.max)
    val (less, more) = nums.toList.partition(_ < high)
    val (list1, list2) = if (less.isEmpty || more.isEmpty) {
      (nums.toList, nums.toList)
    } else {
      (less, more)
    }
    list1.foldLeft(0) { case (max, n1) =>
      math.max(max, list2.foldLeft(0) {
        case (max2, n2) => math.max(max2, n1 ^ n2)
      })
    }
  }

  def highestBit(number: Int): Int = {
    val bit = (0 to 31).find(bit => (1 << bit) >= number)
    bit.map { bit =>
      1 << (bit - 1)
    }.getOrElse {
      1 << 31
    }
  }
}