package leetcode.september2020.week1

import leetcode.september2020.common.TreeNode
import leetcode.september2020.week1.TraverseTreeAndMergeArrays.getAllElements
import org.scalatest._
import flatspec._
import matchers._

class TraverseTreeAndMergeArraysSpec extends AnyFlatSpec with should.Matchers {
  "All Elements in Two Binary Search Trees" should
    "contains all elements in ascending order" in {
    val root1 = TreeNode(2, TreeNode(1), TreeNode(4))
    val root2 = TreeNode(1, TreeNode(0), TreeNode(3))
    getAllElements(root1, root2) shouldBe List(0, 1, 1, 2, 3, 4)

    val root3 = TreeNode(0, TreeNode(-10), TreeNode(10))
    val root4 = TreeNode(5, TreeNode(1, TreeNode(0), TreeNode(2)), TreeNode(7))
    getAllElements(root3, root4) shouldBe List(-10, 0, 0, 1, 2, 5, 7, 10)

    val root5 = null // LeetCode, really null?
    val root6 = TreeNode(5, TreeNode(1, TreeNode(0), TreeNode(2)), TreeNode(7))
    getAllElements(root5, root6) shouldBe List(0, 1, 2, 5, 7)


    val root7 = TreeNode(0, TreeNode(-10), TreeNode(10))
    val root8 = null
    getAllElements(root7, root8) shouldBe List(-10, 0, 10)
  }
}
