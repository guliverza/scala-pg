package leetcode.september2020.common

case class TreeNode(value: Int, left: TreeNode = null, right: TreeNode = null) {
  def isLeaf: Boolean = { left == null && right == null }
}


