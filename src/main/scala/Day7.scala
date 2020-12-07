import scala.io.Source

object Day7 {
  val shinyGoldColor = "shiny gold"
  
  def contains(searchedColor: String, bagColor: String, allBags: Map[String, Array[(String, Int)]]): Boolean = {
    allBags(bagColor)
      .find(bagContent => bagContent._1 == searchedColor || contains(searchedColor, bagContent._1, allBags))
      .isDefined
  }
  
  def part1(bags: Map[String, Array[(String, Int)]]): Unit = {
    val count = bags
      .map(bag => if contains(shinyGoldColor, bag._1, bags) then 1 else 0)
      .sum
    
    println(count)
  }
  
  def countBags(bagColor: String, bags: Map[String, Array[(String, Int)]]): Int = {
    bags(bagColor)
      .map(bagContent => bagContent._2 * (countBags(bagContent._1, bags) + 1))
      .sum
  }
  
  def part2(bags: Map[String, Array[(String, Int)]]): Unit = {
    println(countBags(shinyGoldColor, bags))
  }

  def parseLine(statement: String): (String, Array[(String, Int)]) = {
    val values = statement.trim.split("contain")
    val mainBag = values(0).split("bags")(0).trim
    val content: Array[(String, Int)] = if (values(1).trim == "no other bags.") {
      Array()
    }
    else {
      values(1).trim.dropRight(1).split(",").map(bagString => {
        val splittedValuesBag = bagString.trim.split(" ")

        (splittedValuesBag.drop(1).dropRight(1).mkString(" "), splittedValuesBag(0).toInt)
      })
    }

    (mainBag, content)
  }
  
  def main(args: Array[String]): Unit = {
    val bags: Map[String, Array[(String, Int)]] = Source.fromResource("day7").getLines.map(parseLine(_)).toMap
    
    part2(bags)
  }
}
