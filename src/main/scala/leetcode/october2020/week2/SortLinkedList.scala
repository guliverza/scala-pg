package leetcode.october2020.week2

object SortLinkedList {
  case class ListNode(x: Int = 0, var next: ListNode = null)

  def sortList(head: ListNode): ListNode = {
    mergeSort(head)
  }

  def merge(sorted1: ListNode, sorted2: ListNode): ListNode = {
    var head1 = sorted1
    var head2 = sorted2
    var head: ListNode = null
    var tail: ListNode = null
    while (head1 != null || head2 != null) {
      if (head2 == null || (head1 != null && head1.x < head2.x)) {
        if (tail != null) tail.next = head1
        tail = head1
        head1 = head1.next
      } else {
        if (tail != null) tail.next = head2
        tail = head2
        head2 = head2.next
      }
      if (head == null) head = tail
    }
    head
  }

  def mergeSort(head: ListNode): ListNode = {
    if (head != null && head.next != null) {
      var mid = head
      var tail = head
      var skip = false
      while (tail != null && tail.next != null) {
        if (skip && mid.next != null) mid = mid.next
        skip = !skip
        tail = tail.next
      }
      val head2 = mid.next
      mid.next = null
      val sorted1 = mergeSort(head)
      val sorted2 = mergeSort(head2)
      merge(sorted1, sorted2)
    } else {
      head
    }
  }

  def main(a: Array[String]): Unit = {
    println(sortList(ListNode(12, ListNode(10, ListNode(9, ListNode(8, ListNode(7)))))))
  }
}
