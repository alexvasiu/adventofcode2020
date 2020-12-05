import scala.io.Source
import scala.util.control.Breaks.{breakable, break}

case class Seat(row: Int, column: Int) {
  def getId: Int = row * 8 + column
}

object Day5 {
  def part1(seats: Array[Seat]): Unit = {
    println(seats.map(_.getId).max)
  }
  
  def part2(seats: Array[Seat]): Unit = {
    val sortedSeats: Array[Seat] = seats.sortBy(_.getId)
    
    breakable {
      for (i <- 1 until sortedSeats.length - 1) {
        if (sortedSeats(i).getId + 1 == sortedSeats(i + 1).getId && sortedSeats(i).getId - 2 == sortedSeats(i - 1).getId) {
          println(sortedSeats(i).getId - 1)
          break
        }
        else if (sortedSeats(i).getId - 1 == sortedSeats(i - 1).getId && sortedSeats(i).getId + 2 == sortedSeats(i + 1).getId) {
          println(sortedSeats(i).getId + 1)
          break
        }
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    val seats: Array[Seat] = Source.fromResource("day5").getLines.map(Seat(_)).toArray

    part2(seats)
  }
}

object Seat {
  def apply(id: String): Seat = {
    val row: String = id.substring(0, 7).replace('F', '0').replace('B', '1')
    val column: String = id.substring(7).replace('R', '1').replace('L', '0')
    
    Seat(Integer.parseInt(row, 2), Integer.parseInt(column, 2))
  }
}