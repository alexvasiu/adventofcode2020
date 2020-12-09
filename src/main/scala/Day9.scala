import scala.io.Source
import scala.util.control.Breaks.{breakable, break}

object Day9 {  
  def searchSum(numbers: Array[Long], start: Int, preamble: Int, targetSum: Long): Boolean = {
    (start until start + preamble - 1)
      .exists(i => (start until start + preamble).exists(j => numbers(j) == targetSum - numbers(i)))
  }
  
  def searchNumber(numbers: Array[Long], preamble: Int): Long = {
    (preamble until numbers.length).find(i => !searchSum(numbers, i - preamble, preamble, numbers(i))).headOption match {
      case Some(i) => numbers(i)
      case None => -1
    }
  }
  
  def part1(numbers: Array[Long], preamble: Int): Unit = {
    println(searchNumber(numbers, preamble))
  }
  
  def part2(numbers: Array[Long], preamble: Int): Unit = {
    val wrongNumber = searchNumber(numbers, preamble)
    var start: Int = 0
    var sum: Long = 0
    
    breakable {
      for ((number, index) <- numbers.zipWithIndex) {
        if (number != wrongNumber) {
          if (sum + number <= wrongNumber) {
            if (sum + number == wrongNumber) {
              val subarray = numbers.slice(start, index + 1)
              println(subarray.min + subarray.max)
              break
            }
            else {
              sum += number
            }
          }
          else {
            sum += number
            
            while (sum > wrongNumber) {
              sum -= numbers(start)
              start += 1
            }
            
            if (sum == wrongNumber) {
              val subarray = numbers.slice(start, index + 1)
              println(subarray.min + subarray.max)
              break
            }
          }
        }
        else {
          start = index + 1
          sum = 0
        }
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    val numbers: Array[Long] = Source.fromResource("day9").getLines.map(_.toLong).toArray

    part2(numbers, 25)
  }
}
