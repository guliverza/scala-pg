package leetcode.september2020.week3

import leetcode.september2020.common.BinTree

object MaximumXOR {

  def findMaximumXOR(nums: Array[Int]): Int = {
    println("INPUT: " + nums.toList)
    val bit = highestBit(nums.max)
    println(s"max = ${nums.max}")
    println(s"bit = $bit")
    val root = BinTree(0)
    nums.foreach(num => putBinaryToTree(num, root, bit))
    root.printTree()
    findMax(root.left, root.right, bit)
  }

  def findMax(left: Option[BinTree], right: Option[BinTree], hiBit: Int): Int = (left, right) match {
    case (Some(left), Some(right)) =>
      val bit = 1 << hiBit
      ((left.value & bit) ^ (right.value & bit))+ Seq(
        findMax(left.left, right.left, hiBit - 1),
        findMax(left.left, right.right, hiBit - 1),
        findMax(left.right, right.left, hiBit - 1),
        findMax(left.right, right.right, hiBit - 1),
      ).max
    case (Some(left), _) =>
      findMax(left.left, left.right, hiBit - 1)
    case (_, Some(right)) =>
      findMax(right.left, right.right, hiBit - 1)
    case _ =>
      0
  }

  def highestBit(number: Int): Int = {
    val bit = (0 to 31).find(bit => (1 << bit) > number)
    bit.map { bit =>
      bit-1
    }.getOrElse {
      30
    }
  }

  def putBinaryToTree(num: Int, node: BinTree, hiBit: Int): Unit = {
    if (hiBit >= 0) {
      if ((num & (1 << hiBit)) == 0) {
        node.left = Some(node.left.getOrElse(BinTree(num)))
        putBinaryToTree(num, node.left.get, hiBit - 1)
      } else {
        node.right = Some(node.right.getOrElse(BinTree(num)))
        putBinaryToTree(num, node.right.get, hiBit - 1)
      }
    }
  }
}