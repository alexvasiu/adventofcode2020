import scala.io.Source
import util.control.Breaks.{break, breakable}

object Day1 {
  def binarySearch(values: Array[Int], x: Int): Boolean = {
    var i: Int = 0
    var j: Int = values.length
    
    while (i < j) {
      val m: Int = (i + j) / 2
      
      if (values(m) == x) {
        return true
      }
      else if (values(m) > x) {
        j = m
      }
      else {
        i = m + 1
      }
    }
    
    false
  }
  
  def part1(values: Array[Int]): Unit = {
    breakable {
      for (x <- values) {
        if (binarySearch(values, 2020 - x)) {
          println(x * (2020 - x))
          break
        }
      }
    }
  }
  
  def part2(values: Array[Int]): Unit = {
    breakable {
      for (i <- values.indices) {
        for (j <- i + 1 until values.length) {
          val x: Int = 2020 - values(i) - values(j)
          if (binarySearch(values, x)) {
            println(values(i) * values(j) * x)
            break
          }
        }
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    val values: Array[Int] = Source.fromResource("day1").getLines().map(_.trim.toInt).toArray.sorted
    
    part2(values)
  }
}
