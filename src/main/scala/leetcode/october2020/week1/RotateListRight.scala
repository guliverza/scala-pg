package leetcode.october2020.week1

object RotateListRight {

  case class ListNode(var x: Int = 0, var next: ListNode = null)

  def rotateRight(head: ListNode, k: Int): ListNode = {
    val size = sizeOfList(head)
    if (size < 2 || k % size == 0) {
      head
    } else {
      rotate(head, k % size, size)
    }
  }

  def sizeOfList(root: ListNode): Int = {
    var s = 0
    var node = root
    while (node != null) {
      s += 1
      node = node.next
    }
    s
  }

  def rotate(head: ListNode, k: Int, size: Int): ListNode = {
    val newTail = skip(head, size-k-1)
    val newHead = newTail.next
    val oldTail = findTail(head)
    newTail.next = null
    oldTail.next = head
    newHead
  }

  def skip(head: ListNode, k: Int): ListNode = {
    var i = k
    var res = head
    while (i > 0) {
      res = res.next
      i -= 1
    }
    res
  }

  def findTail(head: ListNode): ListNode = {
    var node = head
    while (node.next != null) node = node.next
    node
  }

  def main(args: Array[String]): Unit = {
    val head = ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5)))))
    println(head)
    println(rotateRight(head, 2))
  }

}
