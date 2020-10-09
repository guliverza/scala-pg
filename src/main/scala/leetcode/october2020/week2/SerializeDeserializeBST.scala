package leetcode.october2020.week2

object SerializeDeserializeBST {
  case class TreeNode(value: Int, left: TreeNode = null, right: TreeNode = null)

  // Encodes a list of strings to a single string.
  def serialize(root: TreeNode): String = {
    if (root == null) "x"
    else root.value + "," + serialize(root.left) + "," + serialize(root.right)
  }

  // Decodes a single string to a list of strings.
  def deserialize(data: String): TreeNode = {
    des(data)._1
  }

  def des(data: String): (TreeNode, String) = {
    val (value, rest) = data.span(_ != ',')
    if (value == "x") (null, rest.tail)
    else {
      val (left, restL) = des(rest.tail)
      val (right, restR) = des(restL)
      TreeNode(value.toInt, left, right) -> restR
    }
  }

  def main(args: Array[String]): Unit = {
    println(deserialize(serialize(TreeNode(7, TreeNode(3, TreeNode(1), TreeNode(5)), TreeNode(8, TreeNode(2), TreeNode(10))))))
    println(deserialize(serialize(null)))
    println(deserialize(serialize(TreeNode(1))))
  }
}
