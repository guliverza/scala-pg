package leetcode.september2020.common

case class BinTree(value: Int, var left: Option[BinTree] = None, var right: Option[BinTree] = None) {
  def print(buffer: StringBuilder, prefix: String, childrenPrefix: String): Unit = {
    buffer.append(prefix)
    buffer.append(value)
    buffer.append('\n')
    val it = (left ++ right).iterator
    while ( {
      it.hasNext
    }) {
      val next = it.next
      if (it.hasNext) next.print(buffer, childrenPrefix + "├── ", childrenPrefix + "│   ")
      else next.print(buffer, childrenPrefix + "└── ", childrenPrefix + "    ")
    }
  }
  def printTree() = {
    val result = new StringBuilder()
    print(result, "", "")
    println(result)
  }
}

object BinTree {
}
