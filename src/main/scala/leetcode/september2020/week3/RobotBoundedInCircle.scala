package leetcode.september2020.week3

import leetcode.september2020.week3.RobotBoundedInCircle.{N, S, W}

object RobotBoundedInCircle {

  sealed class Direction(val goX: Int, val goY: Int, var turnLeft: Direction, var turnRight: Direction)
  case object N extends Direction(0, -1, W, E)
  case object W extends Direction(-1, 0, S, N)
  case object S extends Direction(0, 1, E, W)
  case object E extends Direction(1, 0, N, S)

  def isRobotBounded(instructions: String): Boolean = {
    val initialDirection: Direction = initializeDirections()
    val (direction, x, y) = instructions.foldLeft((initialDirection, 0, 0)) {
      case ((d, x, y), 'G') => (d, x + d.goX, y + d.goY)
      case ((d, x, y), 'L') => (d.turnLeft, x, y)
      case ((d, x, y), 'R') => (d.turnRight, x, y)
      case (x, _) => x
    }
    direction != initialDirection || (x == 0 && y == 0)
  }

  def initializeDirections(): Direction = {
    N.turnLeft = W
    N.turnRight = E
    W.turnLeft = S
    W.turnRight = N
    S.turnLeft = E
    S.turnRight = W
    E.turnLeft = N
    E.turnRight = S
    N
  }

}
