import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

object Day8 {
  def executeProgram(ops: Array[(String, Int)]): (Int, Int) = {
    var accumulator: Int = 0
    var visited: Array[Boolean] = Array.fill(ops.length)(false)
    var index: Int = 0

    breakable {
      while (index != ops.length - 1) {
        if (visited(index)) {
          break
        }

        visited(index) = true

        ops(index)._1 match {
          case "nop" =>
            index += 1
          case "acc" =>
            accumulator += ops(index)._2
            index += 1
          case "jmp" =>
            index += ops(index)._2
        }
      }
    }
    
    (index, accumulator)
  }
  
  def part1(ops: Array[(String, Int)]): Unit = {
    println(executeProgram(ops)._2)
  }
  
  def changeProgram(ops: Array[(String, Int)], index: Int): Array[(String, Int)] = {
    var result = ops.clone()
    result(index) = if (result(index)._1 == "nop") then ("jmp", result(index)._2) else ("nop", result(index)._2)
    result
  }
  
  def part2(ops: Array[(String, Int)]): Unit = {
    breakable {
      for (index <- ops.indices) {
        if (ops(index)._1 == "jmp" || (ops(index)._1 == "nop" && ops(index)._2 != 0)) {
          var (ind, acc): (Int, Int) = executeProgram(changeProgram(ops, index))
          
          if (ind == ops.length - 1) {
            println(acc)
            break
          }
        }
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    val operations: Array[(String, Int)] = Source.fromResource("day8").getLines.map(line => {
      val values = line.trim.split(" ")
      
      (values(0), values(1).toInt)
    }).toArray

    part2(operations)
  }
}
