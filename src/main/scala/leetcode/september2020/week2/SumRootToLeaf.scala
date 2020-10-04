package leetcode.september2020.week2

import common.TreeNode

object SumRootToLeaf {
  /**
   * Given a binary tree, each node has value 0 or 1.
   * Each root-to-leaf path represents a binary number starting with the most significant bit.
   * For example, if the path is 0 -> 1 -> 1 -> 0 -> 1, then this could represent 01101 in binary, which is 13.
   *
   * For all leaves in the tree, consider the numbers represented by the path from the root to that leaf.
   *
   * Return the sum of these numbers.
   *
   * https://leetcode.com/explore/challenge/card/september-leetcoding-challenge/555/week-2-september-8th-september-14th/3453/
   */
  def sumRootToLeaf(root: TreeNode): Int = {
    Option(root).map(infixTraverse(0)).getOrElse(Nil).sum
  }

  def infixTraverse(sum: Int)(node: TreeNode): List[Int] = {
    val result = sum * 2 + node.value
    if (node.isLeaf) {
      List(result)
    } else {
      val left = node.left.map(infixTraverse(result)).getOrElse(Nil)
      val right = node.right.map(infixTraverse(result)).getOrElse(Nil)
      left ++ right
    }
  }

}
