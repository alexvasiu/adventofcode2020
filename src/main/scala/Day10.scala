import scala.io.Source

object Day10 {
  def part1(numbers: Array[Long]): Unit = {
    val count: Int =
      (0 until numbers.length - 1).count(index => numbers(index + 1) - numbers(index) == 1) * 
        (0 until numbers.length - 1).count(index => numbers(index + 1) - numbers(index) == 3)
    
    println(count)
  }
  
  var dpMap: Map[Int, Long] = Map()
  
  def dp(numbes: Array[Long], index: Int): Long = {
    if (index == numbes.length - 1) {
      1
    }
    else if (dpMap.contains(index)) {
      dpMap(index)
    }
    else {
      var sum: Long = 0
      
      for (step <- 1 to 3) {
        if (index + step < numbes.length && numbes(index + step) - numbes(index) <= 3) {
          sum += dp(numbes, index + step)    
        }
      }
      
      dpMap = dpMap.updated(index, sum)
      sum
    }
  }
  
  def part2(numbes: Array[Long]): Unit = {
    println(dp(numbes, 0))
  }
  
  def main(args: Array[String]): Unit = {
    var numbers: Array[Long] = Source.fromResource("day10").getLines.map(_.toLong).toArray
    numbers = (Array(0, numbers.max + 3) ++ numbers).sorted
    
    part2(numbers.dropRight(1))
  }
}
