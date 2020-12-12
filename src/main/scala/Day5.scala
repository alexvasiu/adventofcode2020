import scala.io.Source
import scala.util.control.Breaks.{breakable, break}

object Day5 {
  def part1(seats: Array[Int]): Unit = {
    println(seats.max)
  }
  
  def part2(seats: Array[Int]): Unit = {
    val sortedSeats: Array[Int] = seats.sorted
    
    breakable {
      for (i <- 1 until sortedSeats.length - 1) {
        if (sortedSeats(i) + 1 == sortedSeats(i + 1) && sortedSeats(i) - 2 == sortedSeats(i - 1)) {
          println(sortedSeats(i) - 1)
          break
        }
      }
    }
  }

  def parseId(id: String): Int = {
    Integer.parseInt(id.replace('F', '0').replace('B', '1')
      .replace('R', '1').replace('L', '0'), 2)
  }
  
  def main(args: Array[String]): Unit = {
    val seats: Array[Int] = Source.fromResource("day5").getLines.map(parseId(_)).toArray

    part1(seats)
  }
}