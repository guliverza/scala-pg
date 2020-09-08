package leetcode.september2020.week2
import SumRootToLeaf.sumRootToLeaf
import leetcode.september2020.common.TreeNode
import org.scalatest._

class SumRootToLeafSpec extends FlatSpec with Matchers{
  "SumRootToLeaf" should "find numbers" in {
    val root1 = TreeNode(1, TreeNode(0, TreeNode(0), TreeNode(1)), TreeNode(1, TreeNode(0), TreeNode(1)))
    sumRootToLeaf(root1) shouldBe 22
  }

}
