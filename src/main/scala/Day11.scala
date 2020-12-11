import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

enum SeatType {
  case Empty, Occupied, Floor
}

object Day11 {
  val dx: Array[Int] = Array(-1, 0, 1, 1, 1, 0, -1, -1)
  val dy: Array[Int] = Array(1, 1, 1, 0, -1, -1, -1, 0)
  
  def simulate(seats: Array[Array[SeatType]], countFunction: (Array[Array[SeatType]], Int, Int) => Int,
               occupiedThreshold: Int): Unit = {
    var change: Boolean = true
    var map = seats.clone()
    var newMap = seats.clone()

    while (change) {
      change = false

      (0 until seats.length).foreach(i => (0 until seats(0).length).foreach(j => {
        if (map(i)(j) == SeatType.Empty || map(i)(j) == SeatType.Occupied) {
          var count: Int = countFunction(map, i, j)

          if (map(i)(j) == SeatType.Empty && count == 0) {
            change = true
            newMap = newMap.updated(i, newMap(i).updated(j, SeatType.Occupied))
          }
          else if (map(i)(j) == SeatType.Occupied && count >= occupiedThreshold) {
            change = true
            newMap = newMap.updated(i, newMap(i).updated(j, SeatType.Empty))
          }
        }
      }))

      map = newMap
    }

    println(map.map(row => row.count(x => x == SeatType.Occupied)).sum)
  }
  
  def part1(seats: Array[Array[SeatType]]): Unit = {
    simulate(seats, (map, i, j) => {
      (0 until 8).count(k => {
        val x = i + dx(k)
        val y = j + dy(k)

        x >= 0 && x < map.length && y >= 0 && y < map(0).length && map(x)(y) == SeatType.Occupied
      })
    }, 4)
  }
  
  def part2(seats: Array[Array[SeatType]]): Unit = {
    simulate(seats, (map, i, j) => {
      (0 until 8).count(k => {
        var x: Int = i
        var y: Int = j
        var result: Boolean = false

        breakable {
          while (true) {
            x = x + dx(k)
            y = y + dy(k)

            if (x < 0 || x >= map.length || y < 0 || y >= map(0).length) {
              break
            }

            if (map(x)(y) == SeatType.Occupied) {
              result = true
              break
            }

            if (map(x)(y) == SeatType.Empty) {
              break
            }
          }
        }

        result
      })
    }, 5)
  }
  
  def main(args: Array[String]): Unit = {
    val seats: Array[Array[SeatType]] = Source.fromResource("day11").getLines.map(x => x.strip.split("")
        .map(seat => seat match {
          case "L" => SeatType.Empty
          case "#" => SeatType.Occupied
          case "." => SeatType.Floor
        }).toArray).toArray
    
    part2(seats)
  }
}