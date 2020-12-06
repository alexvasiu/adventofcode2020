import scala.io.Source

object Day6 {
  def part1(groups: List[Array[String]]): Unit = {
    println(groups.map(_.flatMap(_.toArray).toSet.size).sum)
  }
  
  def part2(groups: List[Array[String]]): Unit = {
    println(groups.map(_.map(_.toArray).reduce((acc, element) => acc.intersect(element)).length).sum)
  }

  def main(args: Array[String]): Unit = {
    val groups: List[Array[String]] = Day4.splitArray(Source.fromResource("day6").getLines.map(_.trim).toArray, "")

    part2(groups)
  }
}
