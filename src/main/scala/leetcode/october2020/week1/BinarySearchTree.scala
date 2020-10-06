package leetcode.october2020.week1

object BinarySearchTree {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
      var value: Int = _value
      var left: TreeNode = _left
      var right: TreeNode = _right
    }
  def insertIntoBST(root: TreeNode, value: Int): TreeNode = {
    Option(root).fold(new TreeNode(value)) { root =>
      if (value < root.value) {
        root.left = insertIntoBST(root.left, value)
      } else {
        root.right = insertIntoBST(root.right, value)
      }
      root
    }
  }
}
