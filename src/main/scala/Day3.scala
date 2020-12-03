import scala.io.Source

object Day3 {
  def part1(matrix: Array[Array[Char]], down: Int = 1, right: Int = 3): Int = {
    var i: Int = 0
    var j: Int = 0
    var trees: Int = 0
    
    while (i < matrix.length) {
      if (matrix(i)(j) == '#') {
        trees += 1
      }
      i += down
      j = (j + right) % matrix(0).length
    }
    
    trees
  }
  
  def part2(matrix: Array[Array[Char]]): Unit = {
    println(
      part1(matrix, 1, 1) * 
      part1(matrix, 1, 3) * 
      part1(matrix, 1, 5) * 
      part1(matrix, 1, 7) * 
      part1(matrix, 2, 1)
    )
  }
  
  def main(args: Array[String]): Unit = {
    val matrix: Array[Array[Char]] = Source.fromResource("day3").getLines.map(_.trim.toArray).toArray

    part2(matrix)
  }
}
