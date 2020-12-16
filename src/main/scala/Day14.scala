import scala.io.Source

trait Action
case class Mask(mask: String) extends Action {
  def apply(value: Long): Long = {
    var binaryValue: String = value.toBinaryString
    
    binaryValue = if (binaryValue.length < 36) {
      "0" * (36 - binaryValue.length) + binaryValue
    }
    else {
      binaryValue
    }

    java.lang.Long.parseLong(
      (0 until 36).map(index => {
        if (mask(index) == 'X') {
          binaryValue(index)
        }
        else {
          mask(index)
        }
      }).foldLeft[String]("")(_ + _), 2
    )
  }
  
  def apply2(value: Long): String = {
    var binaryValue: String = value.toBinaryString

    binaryValue = if (binaryValue.length < 36) {
      "0" * (36 - binaryValue.length) + binaryValue
    }
    else {
      binaryValue
    }

    (0 until 36).map(index => {
      if (mask(index) == '0') {
        binaryValue(index)
      }
      else if (mask(index) == '1') {
        '1'
      }
      else {
        'X'
      }
    }).foldLeft[String]("")(_ + _)
  }
}
case class Memory(location: Int, value: Long) extends Action

object Day14 {
  var dictPart2: Map[Long, Long] = Map()
  
  def part1(actions: Array[Action]): Unit = {
    var values: Map[Int, Long] = Map()
    var mask: Mask = Mask("")
    
    for (action <- actions) {
      if (action.isInstanceOf[Mask]) {
        mask = action.asInstanceOf[Mask]
      }
      else {
        val mem: Memory = action.asInstanceOf[Memory]
        values = values.updated(mem.location, mask.apply(mem.value))
      }
    }
    
    println(values.values.sum)
  }
  
  def generateAll(value: String, number: Long, index: Int): Unit = {
   if (index == 36) {
     dictPart2 = dictPart2.updated(java.lang.Long.parseLong(value, 2), number)
   }
   else {
     if (value(index) == 'X') {
       for (i <- 0 to 1) {
         var newValue = s"${value.substring(0, index)}${i.toString}${value.substring(index + 1)}"
         generateAll(newValue, number, index + 1)
       }
     }
     else {
       generateAll(value, number, index + 1)
     }
   }
  }
  
  def part2(actions: Array[Action]): Unit = {
    var mask: Mask = Mask("")

    for (action <- actions) {
      if (action.isInstanceOf[Mask]) {
        mask = action.asInstanceOf[Mask]
      }
      else {
        val mem: Memory = action.asInstanceOf[Memory]
        generateAll(mask.apply2(mem.location), mem.value, 0)
      }
    }

    println(dictPart2.values.sum)
  }
  
  def main(args: Array[String]): Unit = {
    val actions: Array[Action] = Source.fromResource("day14").getLines.map(line => {
      if (line.contains("mask")) {
        Mask(line.split("=")(1).trim)
      }
      else {
        val values = line.split("=")
        Memory(values(0).trim.dropRight(1).drop(4).toInt, values(1).trim.toLong)
      }
    }).toArray
    
    part2(actions)
  }
}