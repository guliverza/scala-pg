package leetcode.september2020.common

case class TreeNode(value: Int, left: Option[TreeNode], right: Option[TreeNode]) {
  def isLeaf: Boolean = left.isEmpty && right.isEmpty
}
object TreeNode {
  def apply(value: Int, left: TreeNode = null, right: TreeNode = null): TreeNode =
    TreeNode(value, Option(left), Option(right))
}
