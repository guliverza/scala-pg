package leetcode.september2020.week1

import common.TreeNode

object TraverseTreeAndMergeArrays {

  def getAllElements(root1: TreeNode, root2: TreeNode): List[Int] = {
    val a1 = Option(root1).map(infixTraverse).getOrElse(Nil)
    val a2 = Option(root2).map(infixTraverse).getOrElse(Nil)
    merge(a1, a2)
  }

  def merge(a1: List[Int], a2: List[Int]): List[Int] = {
    val (res, rest) = a1.foldLeft((List.empty[Int], a2)) {
      case ((a, rest), e1) =>
        val (less, more) = rest.span(_ <= e1)
        (a ++ less :+ e1, more)
    }
    res ++ rest
  }

  def infixTraverse(node: TreeNode): List[Int] = {
    val left: List[Int] = node.left.map(infixTraverse).getOrElse(Nil)
    val right: List[Int] = node.right.map(infixTraverse).getOrElse(Nil)
    (left :+ node.value) ++ right
  }

}
