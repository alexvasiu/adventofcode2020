import scala.io.Source

case class Range(name: String, firstRange: (Int, Int), secondRange: (Int, Int)) {
  def check(value: Int): Boolean = {
    (firstRange._1 <= value && value <= firstRange._2) || (secondRange._1 <= value && value <= secondRange._2)
  }
}

object Day16 {
  def part1(ranges: Array[Range], tickets: Array[Array[Int]]): Unit = {
    println(tickets.map(ticket => ticket.filter(number => ranges.forall(!_.check(number))).sum).sum)
  }
  
  def part2(ranges: Array[Range], myTicket: Array[Int], tickets: Array[Array[Int]]): Unit = {
    val validTickets: Array[Array[Int]] = tickets.filter(ticket => ticket.forall(number => ranges.exists(_.check(number))))
    var result: Array[IndexedSeq[Int]] = ranges.map(range => (0 until ranges.length).filter(index => validTickets.forall(ticket => range.check(ticket(index)))))
    var dict: Map[Int, Int] = Map()
    
    while (result.exists(_.size > 0)) {
      val index = (0 until ranges.length).find(index => result(index).size == 1).get
      val columnValue = result(index)(0)
      dict += (index, columnValue)
      result = result.map(_.filter(_ != columnValue))
    }
    
    println((0 to 5).map(index => myTicket(dict(index)).toLong).reduce(_ * _))
  }
  
  def main(args: Array[String]): Unit = {
    val input: List[Array[String]] = Day4.splitArray(Source.fromResource("day16").getLines.map(_.trim).toArray, "")
    val ranges: Array[Range] = input(0).map(line => {
      val values = line.split(":")
      val rangeValues = values(1).trim.split(" ")
      
      var firstRange = rangeValues(0).split("-")
      var secondRange = rangeValues(2).split("-")
      
      Range(values(0), (firstRange(0).toInt, firstRange(1).toInt), (secondRange(0).toInt, secondRange(1).toInt))
    }).toArray
    val myTicket: Array[Int] = input(1).last.split(",").map(_.toInt).toArray
    val tickets: Array[Array[Int]] = input.last.drop(1).map(_.split(",").map(_.toInt).toArray).toArray
  
    part2(ranges, myTicket, tickets)
  }
}