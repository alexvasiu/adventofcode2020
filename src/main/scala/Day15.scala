import scala.io.Source

object Day15 {
  def calculate(numbers: Array[Long], turn: Int): Long = {
    var last: Long = numbers.last
    var spoken: Map[Long, Int] = numbers.dropRight(1).zipWithIndex.map(x => (x._1, x._2)).toMap
    
    (numbers.length - 1 until turn - 1).foreach(index => {
      if (spoken.contains(last)) {
        val value = spoken(last)
        spoken = spoken.updated(last, index)
        last = index - value
      }
      else {
        spoken = spoken.updated(last, index)
        last = 0
      }
    })
    
    last
  }
  
  def part1(numbers: Array[Long]): Unit = {
    println(calculate(numbers, 2020))
  }

  def part2(numbers: Array[Long]): Unit = {
    println(calculate(numbers, 30000000))
  }
  
  def main(args: Array[String]): Unit = {
    val numbers: Array[Long] = Source.fromResource("day15").getLines.next.split(",").map(_.toLong)
    
    part2(numbers) 
  }
}
