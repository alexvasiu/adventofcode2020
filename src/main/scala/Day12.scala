import scala.io.Source

enum Position {
  case North, South, East, West, Left, Right, Forward
}

object Day12 {
  val positions: List[Position] = List(Position.North, Position.East, Position.South, Position.West)
  
  def getValue(pos: Position, value: Int): (Int, Int) = {
    pos match {
      case Position.East => (value, 0)
      case Position.West => (-value, 0)
      case Position.North => (0, value)
      case Position.South => (0, -value)
      case Position.Forward => (0, 0)
      case Position.Left => (0, 0)
      case Position.Right => (0, 0)
    }
  }
  
  def part1(actions: Array[(Position, Int)]): Unit = {
    var point: (Int, Int) = (0, 0)
    var position: Position = Position.East
    
    for ((action, number) <- actions) {
      if (action == Position.Right) {
        position = positions((positions.indexOf(position, 0) + number / 90) % 4)
      }
      else if (action == Position.Left) {
        position = positions((4 + positions.indexOf(position, 0) - number / 90) % 4)
      }
      else {
        val value = if (action == Position.Forward) {
          getValue(position, number)
        }
        else {
          getValue(action, number)
        }
        point = (point._1 + value._1, point._2 + value._2)
      }
    }
    
    println(point._1.abs + point._2.abs)
  }
  
  def part2(actions: Array[(Position, Int)]): Unit = {
    var point: (Int, Int) = (0, 0)  
    var waypoint: (Int, Int) = (10, 1)

    for ((action, number) <- actions) {
      if (action == Position.Right) {
        (0 until number / 90).foreach(_ => {
          waypoint = (waypoint._2, -waypoint._1)
        })
      }
      else if (action == Position.Left) {
        (0 until number / 90).foreach(_ => {
          waypoint = (-waypoint._2, waypoint._1)
        })
      }
      else if (action == Position.Forward) {
        point = (point._1 + number * waypoint._1, point._2 + number * waypoint._2)
      }
      else {
        val update = getValue(action, number)
        waypoint = (waypoint._1 + update._1, waypoint._2 + update._2)
      }
    }

    println(point._1.abs + point._2.abs)
  }
  
  def main(args: Array[String]): Unit = {
    val actions: Array[(Position, Int)] = Source.fromResource("day12").getLines.map(line => {
      (
        line(0) match {
          case 'N' => Position.North
          case 'S' => Position.South
          case 'E' => Position.East
          case 'W' => Position.West
          case 'L' => Position.Left
          case 'R' => Position.Right
          case 'F' => Position.Forward
        },
        line.drop(1).toInt
      )
    }).toArray

    part2(actions)
  }
}
